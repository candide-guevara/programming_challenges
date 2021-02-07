package receiver

import "context"
import "os"

import "apm_counter/messages"
import "apm_counter/types"
import "apm_counter/util"

type protoApmReceiver struct {
  baseApmReceiver
  batch_size int
}

func NewProtoApmReceiver(conf types.Config) types.ApmReceiver {
  return &protoApmReceiver {
    *newBaseApmReceiver(conf),
    128,
  }
}

func (self *protoApmReceiver) buildBlankTimeserie(conf types.Config) messages.Timeserie {
  ts := messages.Timeserie {}
  ts.Metadata = &messages.Timeserie_Metadata {
    RefSecs: conf.StartTime().Unix(),
    RefNanos: conf.StartTime().UnixNano(),
    PeriodMillis: uint32(conf.OuputPeriod().Milliseconds()),
  }
  ts.OffsetMillis = make([]uint32, self.batch_size)
  ts.KbdCount     = make([]uint32, self.batch_size)
  ts.MseCount     = make([]uint32, self.batch_size)
  ts.BtnCount     = make([]uint32, self.batch_size)
  return ts
}

func (self *protoApmReceiver) clearTimeserie(ts *messages.Timeserie) {
  ts.Metadata = nil
  size := len(ts.OffsetMillis)
  for i:=0; i<size; i++ {
    ts.OffsetMillis[i] = 0
    ts.KbdCount[i] = 0
    ts.MseCount[i] = 0
    ts.BtnCount[i] = 0
  }
}

func (self *protoApmReceiver) truncateTimeserie(ts *messages.Timeserie, size int) {
  ts.OffsetMillis = ts.OffsetMillis[0:size]
  ts.KbdCount     = ts.KbdCount[0:size]
  ts.MseCount     = ts.MseCount[0:size]
  ts.BtnCount     = ts.BtnCount[0:size]
}

func (self *protoApmReceiver) dumpToProtoStream(ctx context.Context, apm_chan <-chan types.ApmBucket) {
  defer self.wait_group.Done()

  var writer *util.FileZipWriter
  self.stats.filepath = util.CreateTimeserieFilename(self.conf, "pb.gz")
  writer, self.stats.err = util.NewFileZipWriter(self.stats.filepath)
  if self.stats.err != nil { return }
  defer writer.Close()
  ts := self.buildBlankTimeserie(self.conf)

  var idx, cnt int
  loop:for {
    select {
      case apm,ok := <-apm_chan:
        self.stats.err = util.ErrOnPrematureClosure(ctx, ok)
        if !ok { break loop }
        //util.Tracef("apm=%v", apm)
        ts.OffsetMillis[idx] = uint32(apm.MillisSince())
        ts.KbdCount[idx]     = uint32(apm.Count(types.ActionKdb))
        ts.MseCount[idx]     = uint32(apm.Count(types.ActionMse))
        ts.BtnCount[idx]     = uint32(apm.Count(types.ActionBtn))
        idx += 1
        if idx == self.batch_size {
          idx = 0
          cnt, self.stats.err = util.WriteProtoWithPrefixedLen(writer, &ts)
          //util.Tracef("ts_batch=%+v", &ts)
          self.clearTimeserie(&ts)
          if self.stats.err != nil { break loop }
          self.stats.filesize += int64(cnt)
        }
      case <-ctx.Done(): break loop
    }
  }
  if self.stats.err == nil && idx != 0 {
    self.truncateTimeserie(&ts, idx)
    cnt, self.stats.err = util.WriteProtoWithPrefixedLen(writer, &ts)
    self.stats.filesize += int64(cnt)
  }
}

func (self *protoApmReceiver) Listen(ctx context.Context, apm_chan <-chan types.ApmBucket) (<-chan bool, error) {
  done_ch := make(chan bool)
  self.wait_group.Add(1)
  if _,err := os.Stat(self.conf.TimeseriesDir()); err != nil { return nil, err }

  go self.dumpToProtoStream(ctx, apm_chan)

  go func() {
    defer close(done_ch)
    self.wait_group.Wait()
    if self.stats.err != nil {
      util.Errorf("Abnormal exit: %v", self.stats.err)
    }
    util.Debugf("%s", self.stats.String())
  }()
  return done_ch, nil
}

