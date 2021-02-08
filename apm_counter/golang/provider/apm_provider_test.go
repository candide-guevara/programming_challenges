package provider

import "context"
import "math"
import "time"
import "testing"

import "apm_counter/types"
import "apm_counter/util"

func collectBucketsFrom(expected_len int, in_ch <-chan types.ApmBucket) <-chan []types.ApmBucket {
  ch := make(chan []types.ApmBucket, 1)
  go func() {
    var buckets []types.ApmBucket
    for b := range in_ch {
      buckets = append(buckets, b)
      if len(buckets) == expected_len { break }
    }
    ch <- buckets
    close(ch)
  }()
  return ch
}

func compareBuckets(t *testing.T, expected_buckets []ApmBucketImpl, buckets []types.ApmBucket) {
  t.Logf("buckets: %v", buckets)
  t.Logf("expected_buckets: %v", expected_buckets)
  if len(expected_buckets) != len(buckets) {
    t.Errorf("mismatched buckets len")
    return
  }
  is_close := func(a, b types.ApmBucket) bool {
    delta := int64(buckets[0].MillisSince()) - int64(expected_buckets[0].MillisSince())
    diff := int64(a.MillisSince()) - int64(b.MillisSince())
    if diff < 0 { diff = -diff }
    if delta < 0 { delta = -delta }
    //t.Logf("diff=%d / delta=%d", diff, delta)
    return math.Abs(float64(diff - delta)) < 0.1 * float64(delta)
  }
  for idx,expect := range expected_buckets {
    real_bucket := buckets[idx].(*ApmBucketImpl)
    if expect.Counts_ != real_bucket.Counts_ {
      t.Errorf("mismatched bucket count")
    }
    if !is_close(real_bucket, &expect) {
      t.Errorf("mismatched bucket time %d: %d - %d ?",
               idx, real_bucket.MillisSince(), expect.MillisSince())
    }
  }
}

func fillBufferedActionChannel(conf types.Config) (chan types.SingleAction, []ApmBucketImpl) {
  period_millis := uint(conf.OutputPeriod().Milliseconds())
  // We rely on the fact the circular buffer is len=12 and the window_len=10
  // The followin actions will fill the first 3 buckets
  // However the third will immediately be decremented since it is the tail
  actions := []types.SingleAction {
    {0, types.ActionKdb},
    {0, types.ActionMse},
    {period_millis - 1, types.ActionKdb},
    {period_millis + 1, types.ActionKdb},
    {2*period_millis + 1, types.ActionKdb},
  }
  var expected_buckets []ApmBucketImpl
  win_len := int(conf.WindowsDuration() / conf.OutputPeriod())
  first_win_bucket := ApmBucketImpl{BucketT{[types.ActionCnt]uint{3,1,0}}, 0}
  for i:=0; i<win_len; i++ {
    first_win_bucket.MillisSince_ = uint(i) * period_millis
    expected_buckets = append(expected_buckets, first_win_bucket)
  }
  bucket_millis := first_win_bucket.MillisSince_ + period_millis
  expected_buckets = append(expected_buckets,
                            ApmBucketImpl{BucketT{[types.ActionCnt]uint{1,0,0}}, bucket_millis})
  bucket_millis += period_millis
  expected_buckets = append(expected_buckets,
                            ApmBucketImpl{BucketT{[types.ActionCnt]uint{0,0,0}}, bucket_millis})

  ch := make(chan types.SingleAction, len(actions))
  for _,a := range actions { ch <- a }
  return ch, expected_buckets
}

func apmProviderTestSetup() (context.Context, context.CancelFunc, *util.ConfigImpl) {
  conf := util.NewTestConfig()
  conf.RefTime_ = conf.StartTime().Truncate(time.Second)
  conf.WindowDuration_ = 200 * time.Millisecond
  conf.OutputPeriod_   = 20 * time.Millisecond
  util.InitLogging(conf)
  ctx, cancel := context.WithCancel(context.Background())
  return ctx, cancel, conf
}

func TestApmProvider(t *testing.T) {
  var err error
  ctx, cancel, conf := apmProviderTestSetup()
  in_ev, expected_buckets := fillBufferedActionChannel(conf)

  var in_apm <-chan types.ApmBucket
  apms := NewApmProvider(conf).(*apmProviderImpl)
  in_apm, err = apms.AggregateEvents(ctx, in_ev)
  if err != nil { t.Fatalf("could not aggregate actions: %v", err) }

  out_buckets := collectBucketsFrom(len(expected_buckets), in_apm)
  test_duration := time.Duration(len(expected_buckets)) * conf.OutputPeriod()
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
  conf := util.NewTestConfig()
  conf.RefTime_ = conf.StartTime().Add(-10 * time.Millisecond)
  action := types.SingleAction{ 0, types.ActionMse, }
  apms := NewApmProvider(conf).(*apmProviderImpl)
  delay := apms.checkForDelayInFilling(action)
  if delay - 10 > 1 {
    t.Errorf("bad delay calculation")
  }
}

func TestCheckForDelayInCleaning(t *testing.T) {
  _,_,conf := apmProviderTestSetup()
  conf.RefTime_ = time.Now().Add(-6 * conf.OutputPeriod())
  apms := NewApmProvider(conf).(*apmProviderImpl)
  new_tail := apms.checkForDelayInCleaning(4)
  if new_tail != 8 {
    t.Errorf("bad tail correction after delay")
  }
}

