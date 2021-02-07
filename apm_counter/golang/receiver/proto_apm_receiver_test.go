package receiver

import "testing"
import "time"

import "apm_counter/messages"

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

