package provider

import "context"
import "fmt"
import "sync"
import "time"

import "apm_counter/types"
import "apm_counter/util"

type BucketT struct {
  Counts_ [types.ActionCnt]uint
}

type ApmBucketImpl struct {
  BucketT
  MillisSince_ uint
}

func (self *ApmBucketImpl) MillisSince() uint { return self.MillisSince_ }
func (self *ApmBucketImpl) Count(action types.ActionT) uint { return self.Counts_[action] }

func (self *ApmBucketImpl) String() string {
  return fmt.Sprintf("Counts=%v, time=%d",
                     self.Counts_, self.MillisSince_)
}

func (self *BucketT) update(ev types.SingleAction) {
  self.Counts_[ev.ActionCode()] += 1
}

func (self *BucketT) add(b BucketT) {
  for idx,v := range b.Counts_ {
    self.Counts_[idx] += v
  }
}

func (self *BucketT) sub(b BucketT) {
  for idx,v := range b.Counts_ {
    if v > self.Counts_[idx] { panic("v > self.Counts[idx]") }
    self.Counts_[idx] -= v
  }
}

func (self *BucketT) reset() {
  for idx,_ := range self.Counts_ {
    self.Counts_[idx] = 0
  }
}

type apmProviderStats struct {
  err error
  buckets_sent uint
  buckets_dropped uint
  untimely_cleaned_buckets uint
  cum_clean_fill_millis uint
  cum_fill_delay_millis uint
}

type apmProviderImpl struct {
  conf types.Config
  ev_time_ref time.Time
  window_millis uint
  period_millis uint
  circ_buffer []BucketT
  accumulator BucketT
  decumulator BucketT
  stats apmProviderStats
  wait_group *sync.WaitGroup
}

func (self *apmProviderStats) String() string {
  return fmt.Sprintf(`
  buckets_sent    = %d
  buckets_dropped = %d
  untimely_cleaned_buckets = %d
  cum_clean_fill_millis = %d
  cum_fill_delay_millis = %d
  err = '%v'`,
  self.buckets_sent,
  self.buckets_dropped,
  self.untimely_cleaned_buckets,
  self.cum_clean_fill_millis,
  self.cum_fill_delay_millis,
  self.err)
}

func NewApmProvider(conf types.Config) types.ApmProvider {
  if conf.WindowsDuration() % conf.OutputPeriod() != 0 {
    panic("Output period must be multiple of a minute")
  }
  buckets_per_min := conf.WindowsDuration() / conf.OutputPeriod()
  return &apmProviderImpl{
    conf,
    conf.StartTime(),
    uint(conf.WindowsDuration().Milliseconds()),
    uint(conf.OutputPeriod().Milliseconds()),
    // zero initialized ?
    make([]BucketT, (12 * buckets_per_min / 10)),
    BucketT{},
    BucketT{},
    apmProviderStats{},
    new(sync.WaitGroup),
  }
}

func (self *apmProviderImpl) calculateApmBucket(tick time.Time) types.ApmBucket {
  b := ApmBucketImpl{
    self.accumulator,
    uint(tick.Sub(self.ev_time_ref).Milliseconds()),
  }
  b.sub(self.decumulator)
  return &b
}

func (self *apmProviderImpl) checkForDelayInFilling(ev types.SingleAction) uint {
  ev_time := self.ev_time_ref.Add(ev.DurationSince())
  delta_millis := uint(time.Since(ev_time).Milliseconds())
  self.stats.cum_fill_delay_millis += delta_millis
  if delta_millis > (self.period_millis / 2) {
    util.Warnf("Filling buckets from events with a %d ms delay", delta_millis)
  }
  return delta_millis
}

func (self *apmProviderImpl) checkForDelayInCleaning(cur_tail uint) uint {
  buf_len := uint(len(self.circ_buffer))
  cur_tail_no_offset := cur_tail + (self.window_millis / self.period_millis) - buf_len
  offset_duration := time.Duration(cur_tail_no_offset * self.period_millis) * time.Millisecond
  tail_time := self.ev_time_ref.Add(offset_duration)
  delta_millis := time.Since(tail_time).Milliseconds()

  if delta_millis < 0 {
    util.Warnf("Ticker is ticking too soon by %d ms", delta_millis)
    return cur_tail
  }

  missing_cleaning := (uint(delta_millis) / self.period_millis)
  self.stats.cum_clean_fill_millis += uint(delta_millis)
  self.stats.untimely_cleaned_buckets += missing_cleaning

  for i := uint(0); i < missing_cleaning; i++ {
    tail_idx := (cur_tail + i) % buf_len
    util.Warnf("Untimely cleaned self.circ_buffer[%d]", tail_idx)
    self.circ_buffer[tail_idx].reset()
  }
  return cur_tail + missing_cleaning
}

func (self *apmProviderImpl) fillHeadAndAccumulatorBuckets(ctx context.Context, ev_chan <-chan types.SingleAction) {
  const check_period = 128
  defer self.wait_group.Done()
  buf_len := uint(len(self.circ_buffer))
  check_counter := 0

  for {
    select {
      case ev,ok := <-ev_chan:
        self.stats.err = util.ErrOnPrematureClosure(ctx, ok)
        if !ok { return }
        head := (ev.MillisSince() / self.period_millis) % buf_len
        self.circ_buffer[head].update(ev)
        self.accumulator.update(ev)
        //util.Tracef("ev=%v, bucket=%v, head=%d", ev, self.circ_buffer[head], head)
        check_counter += 1
        if check_counter % check_period == 0 {
          self.checkForDelayInFilling(ev)
        }
      case <-ctx.Done(): return
    }
  }
}

func (self *apmProviderImpl) cleanTailAndSendApm(ctx context.Context, out chan<- types.ApmBucket) {
  const check_period = 64
  period_tick := time.NewTicker(time.Duration(self.period_millis) * time.Millisecond)
  defer period_tick.Stop()
  defer self.wait_group.Done()
  buf_len := uint(len(self.circ_buffer))
  tail := buf_len - (self.window_millis / self.period_millis)
  check_counter := 0

  for {
    select {
      case bucket_time := <-period_tick.C:
        self.stats.buckets_sent += 1
        tail_idx := tail % buf_len
        tail += 1
        self.decumulator.add(self.circ_buffer[tail_idx])
        self.circ_buffer[tail_idx].reset()
        if len(out) > 0 {
          util.Warnf("Channel full, dropping bucket %v", self.calculateApmBucket(bucket_time))
          self.stats.buckets_dropped += 1
          continue
        }

        b := self.calculateApmBucket(bucket_time)
        //util.Tracef("tail_idx=%d, bucket=%v, acc=%v, dec=%v", tail_idx, b, self.accumulator, self.decumulator)
        out <- b

        check_counter += 1
        if check_counter % check_period == 0 {
          tail = self.checkForDelayInCleaning(tail)
        }
      case <-ctx.Done(): return
    }
  }
}

func (self *apmProviderImpl) AggregateEvents(ctx context.Context, ev_chan <-chan types.SingleAction) (<-chan types.ApmBucket, error) {
  out := make(chan types.ApmBucket, 1)
  self.wait_group.Add(2)

  go self.fillHeadAndAccumulatorBuckets(ctx, ev_chan)
  go self.cleanTailAndSendApm(ctx, out)

  go func() {
    defer close(out)
    self.wait_group.Wait()
    if self.stats.err != nil {
      util.Errorf("Abnormal exit: %v", self.stats.err)
    }
    util.Debugf("Total APM = %v", self.accumulator)
    util.Debugf("%s", self.stats.String())
  }()
  return out, nil
}

