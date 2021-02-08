package receiver

import "context"
import "fmt"
import "io"
import "os"

import "apm_counter/types"
import "apm_counter/util"

type csvApmReceiver struct {
  baseApmReceiver
}

func NewCsvApmReceiver(conf types.Config) types.ApmReceiver {
  return &csvApmReceiver { *newBaseApmReceiver(conf) }
}

func (self *csvApmReceiver) createNewCvsFile() (*os.File, error) {
  file, err := util.CreateTimeserieFile(self.conf, "csv")
  if err == nil {
    header := fmt.Sprintf("%s,%s,%s\n",
                          types.ActionKdb.Name(),
                          types.ActionMse.Name(),
                          types.ActionBtn.Name())
    if _,err := file.WriteString(header); err != nil {
      file.Close()
    }
  }
  return file, err
}

func (self *csvApmReceiver) dumpToCvsFile(ctx context.Context, apm_chan <-chan types.ApmBucket) {
  defer self.wait_group.Done()

  var file *os.File
  file, self.stats.err = self.createNewCvsFile()
  if self.stats.err != nil { return }
  self.stats.filepath = file.Name()
  defer file.Close()

  loop:for {
    select {
      case apm,ok := <-apm_chan:
        self.stats.err = util.ErrOnPrematureClosure(ctx, ok)
        if !ok { break loop }
        //util.Tracef("apm=%v", apm)
        line := fmt.Sprintf("%d,%d,%d\n",
                            apm.Count(types.ActionKdb),
                            apm.Count(types.ActionMse),
                            apm.Count(types.ActionBtn))
        _,self.stats.err = file.WriteString(line)
        if self.stats.err != nil { break loop }
      case <-ctx.Done(): break loop
    }
  }
  if self.stats.err == nil {
    self.stats.filesize, self.stats.err = file.Seek(0, io.SeekCurrent)
  }
}

func (self *csvApmReceiver) Listen(ctx context.Context, apm_chan <-chan types.ApmBucket) (<-chan bool, error) {
  done_ch := make(chan bool)
  self.wait_group.Add(1)
  if _,err := os.Stat(self.conf.TimeserieDir()); err != nil { return nil, err }

  go self.dumpToCvsFile(ctx, apm_chan)

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

