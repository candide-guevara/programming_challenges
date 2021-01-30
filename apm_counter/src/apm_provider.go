package main

import "context"
import "fmt"
import "sync"
import "time"

type bucket struct {
  counts [ActionCnt]uint
}

type apmBucketImpl struct {
  bucket
  end_time time.Time
}

func (self *apmBucketImpl) Time() time.Time { return self.end_time }
func (self *apmBucketImpl) Count(action ActionT) uint { return self.counts[action] }

func (self *apmBucketImpl) String() string {
  return fmt.Sprintf("counts=%v, time=%s", self.counts,
                     self.end_time.Format("15:04:05.000"))
}

func (self *bucket) update(ev SingleAction) {
  self.counts[ev.ActionCode()] += 1
}

func (self *bucket) add(b bucket) {
  for idx,v := range b.counts {
    self.counts[idx] += v
  }
}

func (self *bucket) sub(b bucket) {
  for idx,v := range b.counts {
    if v > self.counts[idx] { panic("v > self.counts[idx]") }
    self.counts[idx] -= v
  }
}

func (self *bucket) reset() {
  for idx,_ := range self.counts {
    self.counts[idx] = 0
  }
}

type apmProviderStats struct {
  buckets_sent uint
  buckets_dropped uint
  untimely_cleaned_buckets uint
  cum_fill_delay_millis uint
  cum_clean_fill_millis uint
}

type apmProviderImpl struct {
  conf Config
  ev_time_ref time.Time
  window_millis uint
  period_millis uint
  circ_buffer []bucket
  accumulator bucket
  decumulator bucket
  stats apmProviderStats
  wait_group *sync.WaitGroup
}

func NewApmProvider(conf Config) ApmProvider {
  min_as_millis := uint((1 * time.Minute).Milliseconds())
  if min_as_millis % conf.OuputPeriodMillis() != 0 {
    panic("Output period must be multiple of a minute")
  }
  buckets_per_min := min_as_millis / conf.OuputPeriodMillis()
  return &apmProviderImpl{
    conf,
    conf.StartTime(),
    min_as_millis,
    conf.OuputPeriodMillis(),
    // zero initialized ?
    make([]bucket, (12 * buckets_per_min / 10)),
    bucket{},
    bucket{},
    apmProviderStats{},
    new(sync.WaitGroup),
  }
}

func (self *apmProviderImpl) calculateApmBucket(tick time.Time) ApmBucket {
  b := apmBucketImpl{
    self.accumulator,
    tick,
  }
  b.sub(self.decumulator)
  return &b
}

func (self *apmProviderImpl) checkForDelayInFilling(ev SingleAction) {
  ev_time := self.ev_time_ref.Add(ev.DurationSince())
  delta_millis := uint(time.Since(ev_time).Milliseconds())
  self.stats.cum_fill_delay_millis += delta_millis
  if delta_millis > (self.period_millis / 2) {
    Warnf("Filling buckets from events with a %d ms delay", delta_millis)
  }
}

func (self *apmProviderImpl) checkForDelayInCleaning(cur_tail uint) uint {
  buf_len := uint(len(self.circ_buffer))
  cur_tail_no_offset := cur_tail + (self.window_millis / self.period_millis) - buf_len
  offset_duration := time.Duration(cur_tail_no_offset * self.period_millis) * time.Millisecond
  tail_time := self.ev_time_ref.Add(offset_duration)
  delta_millis := time.Since(tail_time).Milliseconds()

  if delta_millis < 0 {
    Warnf("Ticker is ticking too soon by %d ms", delta_millis)
    return cur_tail
  }

  missing_cleaning := (uint(delta_millis) / self.period_millis)
  self.stats.cum_clean_fill_millis += uint(delta_millis)
  self.stats.untimely_cleaned_buckets += missing_cleaning

  for i := uint(0); i < missing_cleaning; i++ {
    tail_idx := (cur_tail + i) % buf_len
    Warnf("Untimely cleaned self.circ_buffer[%d]", tail_idx)
    self.circ_buffer[tail_idx].reset()
  }
  return cur_tail + missing_cleaning
}

func (self *apmProviderImpl) fillHeadAndAccumulatorBuckets(ctx context.Context, ev_chan <-chan SingleAction) {
  const check_period = 128
  defer self.wait_group.Done()
  buf_len := uint(len(self.circ_buffer))
  check_counter := 0

  for {
    select {
      case ev,ok := <-ev_chan:
        if !ok { return }
        head := (ev.MillisSince() / self.period_millis) % buf_len
        //Tracef("head=%d", head)
        self.circ_buffer[head].update(ev)
        self.accumulator.update(ev)
        check_counter += 1
        if check_counter % check_period == 0 {
          self.checkForDelayInFilling(ev)
        }
      case <-ctx.Done(): return
    }
  }
}

func (self *apmProviderImpl) cleanTailAndSendApm(ctx context.Context, out chan<- ApmBucket) {
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
          Warnf("Channel full, dropping bucket %v", self.calculateApmBucket(bucket_time))
          self.stats.buckets_dropped += 1
          continue
        }

        b := self.calculateApmBucket(bucket_time)
        //Tracef("tail_idx=%d, buf_len=%d, bucket=%v", tail_idx, buf_len, b)
        out <- b

        check_counter += 1
        if check_counter % check_period == 0 {
          tail = self.checkForDelayInCleaning(tail)
        }
      case <-ctx.Done(): return
    }
  }
}

func (self *apmProviderImpl) AggregateEvents(ctx context.Context, ev_chan <-chan SingleAction) (<-chan ApmBucket, error) {
  out := make(chan ApmBucket, 1)
  self.wait_group.Add(2)

  go self.fillHeadAndAccumulatorBuckets(ctx, ev_chan)
  go self.cleanTailAndSendApm(ctx, out)

  go func() {
    self.wait_group.Wait()
    Debugf("Total APM = %v", self.accumulator)
    Debugf("%+v", self.stats)
    close(out)
  }()
  return out, nil
}

