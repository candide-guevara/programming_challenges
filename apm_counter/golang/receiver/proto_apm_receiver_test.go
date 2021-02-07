package receiver

import "context"
import "errors"
import "io"
import "os"
import "testing"
import "time"

import "apm_counter/messages"
import "apm_counter/provider"
import "apm_counter/types"
import "apm_counter/util"

import "google.golang.org/protobuf/proto"

func fillApmsAndExpectedProtoMultiBatch(t *testing.T, conf types.Config, batch_size int, batch_cnt int) (chan types.ApmBucket, []*messages.Timeserie) {
  ch_len := batch_size * batch_cnt
  ch := make(chan types.ApmBucket, ch_len)
  expected_pbs := make([]*messages.Timeserie, batch_cnt)
  for i:=0; i<ch_len; i++ {
    apm := provider.ApmBucketImpl{provider.BucketT{[types.ActionCnt]uint{1,1,1}}, 10}
    ch <- &apm
  }
  for i:=0; i<batch_cnt; i++ {
    expected_pb := messages.Timeserie {
      Metadata:     nil,
      OffsetMillis: make([]uint32, batch_size),
      KbdCount:     make([]uint32, batch_size),
      MseCount:     make([]uint32, batch_size),
      BtnCount:     make([]uint32, batch_size),
    }
    if i == 0 {
      expected_pb.Metadata = &messages.Timeserie_Metadata {
        RefSecs: conf.StartTime().Unix(),
        RefNanos: conf.StartTime().UnixNano(),
        PeriodMillis: uint32(conf.OuputPeriod().Milliseconds()),
      }
    }
    for j:=0; j<batch_size; j++ {
      expected_pb.OffsetMillis[j] = 10
      expected_pb.KbdCount[j] = 1
      expected_pb.MseCount[j] = 1
      expected_pb.BtnCount[j] = 1
    }
    expected_pbs[i] = &expected_pb
  }
  return ch, expected_pbs
}

func fillApmsAndExpectedProto(t *testing.T, conf types.Config) (chan types.ApmBucket, *messages.Timeserie) {
  apms := []provider.ApmBucketImpl {
    {provider.BucketT{[types.ActionCnt]uint{0,0,0}}, 10},
    {provider.BucketT{[types.ActionCnt]uint{3,1,0}}, 20},
    {provider.BucketT{[types.ActionCnt]uint{2,1,0}}, 30},
    {provider.BucketT{[types.ActionCnt]uint{2,3,0}}, 40},
  }
  expected_pb := messages.Timeserie {
    Metadata: &messages.Timeserie_Metadata {
      RefSecs: conf.StartTime().Unix(),
      RefNanos: conf.StartTime().UnixNano(),
      PeriodMillis: uint32(conf.OuputPeriod().Milliseconds()),
    },
    OffsetMillis: []uint32{10, 20, 30, 40},
    KbdCount:     []uint32{0, 3, 2, 2},
    MseCount:     []uint32{0, 1, 1, 3},
    BtnCount:     []uint32{0, 0, 0, 0},
  }
  ch := make(chan types.ApmBucket, len(apms))
  for i := range apms { ch <- &apms[i] }
  return ch, &expected_pb
}

func compareFileContentToProto(t *testing.T, filepath string, expected_pbs []*messages.Timeserie) {
  reader, err := util.NewFileZipReader(filepath)
  if err != nil {
    t.Errorf("Timeseries file could not be read: %v", err)
  }
  var read int
  var ts messages.Timeserie
  for idx,expected_pb := range expected_pbs {
    read,err = util.ReadProtoWithPrefixedLen(reader, &ts)
    if err != nil && !errors.Is(err, io.EOF) { t.Fatalf("%v", err) }

    if !proto.Equal(&ts, expected_pb) {
      t.Errorf("idx=%d, read=%d\n%v !=\n%v", idx, read, &ts, expected_pb)
    }
    proto.Reset(&ts)
  }
  read,err = util.ReadProtoWithPrefixedLen(reader, &ts)
  if !errors.Is(err, io.EOF) { t.Errorf("Expected EOF after read") }
}

func protoApmReceiverTestSetup() (context.Context, context.CancelFunc, *util.ConfigImpl) {
  conf := util.NewTestConfig()
  conf.TimeseriesDir_ = os.Getenv("TEMP")
  util.InitLogging(conf)
  ctx, cancel := context.WithCancel(context.Background())
  return ctx, cancel, conf
}

func TestProtoApmRecieverListen(t *testing.T) {
  var err error
  ctx, cancel, conf := protoApmReceiverTestSetup()
  in_apm, expected_pb := fillApmsAndExpectedProto(t, conf)

  var done_ch <-chan bool
  dumper := NewProtoApmReceiver(conf).(*protoApmReceiver)
  done_ch, err = dumper.Listen(ctx, in_apm)
  if err != nil { t.Fatalf("could not dump to proto: %v", err) }

  time.Sleep(10 * time.Millisecond)
  cancel()
  close(in_apm)
  select {
    case <-done_ch: break
    case <-time.After(10 * time.Millisecond):
      t.Fatalf("Receiver could not finish before the timeout")
  }
  expected_pbs := []*messages.Timeserie{expected_pb}
  compareFileContentToProto(t, dumper.stats.filepath, expected_pbs)
  if dumper.stats.err != nil {
    t.Errorf("Receiver ended with error: %v", dumper.stats.err)
  }
}

func TestProtoApmRecieverListenMultiBatch(t *testing.T) {
  const batch_size = 16
  var err error
  ctx, cancel, conf := protoApmReceiverTestSetup()
  in_apm, expected_pbs := fillApmsAndExpectedProtoMultiBatch(t, conf, batch_size, 3)

  var done_ch <-chan bool
  dumper := NewProtoApmReceiver(conf).(*protoApmReceiver)
  dumper.batch_size = batch_size
  done_ch, err = dumper.Listen(ctx, in_apm)
  if err != nil { t.Fatalf("could not dump to proto: %v", err) }

  time.Sleep(10 * time.Millisecond)
  cancel()
  close(in_apm)
  select {
    case <-done_ch: break
    case <-time.After(50 * time.Millisecond):
      t.Fatalf("Receiver could not finish before the timeout")
  }
  compareFileContentToProto(t, dumper.stats.filepath, expected_pbs)
  if dumper.stats.err != nil {
    t.Errorf("Receiver ended with error: %v", dumper.stats.err)
  }
}

