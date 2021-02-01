package main

import "context"
import "time"
import "testing"

func collectBucketsFrom(expected_len int, in_ch <-chan ApmBucket) <-chan []ApmBucket {
  ch := make(chan []ApmBucket, 1)
  go func() {
    var buckets []ApmBucket
    for b := range in_ch {
      buckets = append(buckets, b)
      if len(buckets) == expected_len { break }
    }
    ch <- buckets
    close(ch)
  }()
  return ch
}

func compareBuckets(t *testing.T, expected_buckets []apmBucketImpl, buckets []ApmBucket) {
  t.Logf("buckets: %v", buckets)
  t.Logf("expected_buckets: %v", expected_buckets)
  if len(expected_buckets) != len(buckets) {
    t.Errorf("mismatched buckets len")
    return
  }
  delta := buckets[0].MillisSince() - expected_buckets[0].MillisSince()
  for idx,expect := range expected_buckets {
    real_bucket := buckets[idx].(*apmBucketImpl)
    if expect.counts != real_bucket.counts {
      t.Errorf("mismatched bucket count")
    }
    new_delta := real_bucket.MillisSince() - expect.MillisSince()
    if new_delta != delta {
      t.Errorf("mismatched bucket time")
    }
  }
}

func fillBufferedActionChannel(conf Config) (chan SingleAction, []apmBucketImpl) {
  period_millis := uint(conf.OuputPeriod().Milliseconds())
  // We rely on the fact the circular buffer is len=12 and the window_len=10
  // The followin actions will fill the first 3 buckets
  // However the third will immediately be decremented since it is the tail
  actions := []SingleAction {
    {0, ActionKdb},
    {0, ActionMse},
    {period_millis - 1, ActionKdb},
    {period_millis + 1, ActionKdb},
    {2*period_millis + 1, ActionKdb},
  }
  var expected_buckets []apmBucketImpl
  win_len := int(conf.WindowsDuration() / conf.OuputPeriod())
  first_win_bucket := apmBucketImpl{bucketT{[ActionCnt]uint{3,1,0}}, 0}
  for i:=0; i<win_len; i++ {
    first_win_bucket.millis_since = uint(i) * period_millis
    expected_buckets = append(expected_buckets, first_win_bucket)
  }
  bucket_millis := first_win_bucket.millis_since + period_millis
  expected_buckets = append(expected_buckets,
                            apmBucketImpl{bucketT{[ActionCnt]uint{1,0,0}}, bucket_millis})
  bucket_millis += period_millis
  expected_buckets = append(expected_buckets,
                            apmBucketImpl{bucketT{[ActionCnt]uint{0,0,0}}, bucket_millis})

  ch := make(chan SingleAction, len(actions))
  for _,a := range actions { ch <- a }
  return ch, expected_buckets
}

func apmProviderTestSetup() (context.Context, context.CancelFunc, *ConfigImpl) {
  conf := NewTestConfig()
  conf.ref_time = conf.StartTime().Truncate(time.Second)
  conf.window_duration = 200 * time.Millisecond
  conf.output_period   = 20 * time.Millisecond
  InitLogging(conf)
  ctx, cancel := context.WithCancel(context.Background())
  return ctx, cancel, conf
}

func TestApmProvider(t *testing.T) {
  var err error
  ctx, cancel, conf := apmProviderTestSetup()
  in_ev, expected_buckets := fillBufferedActionChannel(conf)

  var in_apm <-chan ApmBucket
  apms := NewApmProvider(conf).(*apmProviderImpl)
  in_apm, err = apms.AggregateEvents(ctx, in_ev)
  if err != nil { t.Fatalf("could not aggregate actions: %v", err) }

  out_buckets := collectBucketsFrom(len(expected_buckets), in_apm)
  test_duration := time.Duration(len(expected_buckets)) * conf.OuputPeriod()
  test_duration += 10 * time.Millisecond

  select {
    case buckets := <-out_buckets:
      cancel()
      close(in_ev)
      compareBuckets(t, expected_buckets, buckets)
    case <-time.After(test_duration):
      t.Fatalf("Failed to collect enough before timeout")
  }
  select {
    case _,ok := <-in_apm:
      if ok { t.Fatalf("Did not consume all actions in the pipe") }
    case <-time.After(100 * time.Millisecond):
      t.Fatalf("Failed to close apm bucket channel before timeout")
  }
  if apms.stats.err != nil {
    t.Errorf("Apm provider ended with error: %v", apms.stats.err)
  }
}

func TestCheckForDelayInFilling(t *testing.T) {
  conf := NewTestConfig()
  conf.ref_time = conf.StartTime().Add(-10 * time.Millisecond)
  action := SingleAction{ 0, ActionMse, }
  apms := NewApmProvider(conf).(*apmProviderImpl)
  delay := apms.checkForDelayInFilling(action)
  if delay - 10 > 1 {
    t.Errorf("bad delay calculation")
  }
}

func TestCheckForDelayInCleaning(t *testing.T) {
  _,_,conf := apmProviderTestSetup()
  conf.ref_time = time.Now().Add(-6 * conf.OuputPeriod())
  apms := NewApmProvider(conf).(*apmProviderImpl)
  new_tail := apms.checkForDelayInCleaning(4)
  if new_tail != 8 {
    t.Errorf("bad tail correction after delay")
  }
}

