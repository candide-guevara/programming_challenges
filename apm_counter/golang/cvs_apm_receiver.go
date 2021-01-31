package main

import "context"
import "fmt"
import "io"
import "os"
import fpmod "path/filepath"
import "sync"

type csvApmReceiverStats struct {
  err error
  filepath string
  filesize int64
}

type csvApmReceiver struct {
  conf Config
  stats csvApmReceiverStats
  wait_group *sync.WaitGroup
}

func NewCsvApmReceiver(conf Config) ApmReceiver {
  return &csvApmReceiver {
    conf,
    csvApmReceiverStats{},
    new(sync.WaitGroup),
  }
}

func (self *csvApmReceiverStats) String() string {
  return fmt.Sprintf(`
  filepath = '%s'
  filesize = %.2e
  err      = '%v'`,
  self.filepath,
  float32(self.filesize),
  self.err)
}

func (self *csvApmReceiver) createNewCvsFile() (*os.File, error) {
  datestr := self.conf.StartTime().Format("02-01-2006-15-04")
  filepath := fpmod.Join(self.conf.TimeseriesDir(),
                         fmt.Sprintf("timeseries-%s.csv", datestr))
  self.stats.filepath = filepath
  file, err := os.Create(filepath)
  if err == nil {
    header := fmt.Sprintf("%s,%s,%s\n",
                          ActionKdb.Name(), ActionMse.Name(), ActionBtn.Name())
    if _,err := file.WriteString(header); err != nil {
      file.Close()
    }
  }
  return file, err
}

func (self *csvApmReceiver) dumpToCvsFile(ctx context.Context, apm_chan <-chan ApmBucket) {
  defer self.wait_group.Done()

  var file *os.File
  file, self.stats.err = self.createNewCvsFile()
  if self.stats.err != nil { return }
  defer file.Close()

  loop:for {
    select {
      case apm,ok := <-apm_chan:
        self.stats.err = ErrOnPrematureClosure(ctx, ok)
        if self.stats.err != nil { break loop }
        line := fmt.Sprintf("%d,%d,%d\n",
                            apm.Count(ActionKdb), apm.Count(ActionMse), apm.Count(ActionBtn))
        _,self.stats.err = file.WriteString(line)
        if self.stats.err != nil { break loop }
      case <-ctx.Done(): break loop
    }
  }
  if self.stats.err == nil {
    self.stats.filesize, self.stats.err = file.Seek(0, io.SeekCurrent)
  }
}

func (self *csvApmReceiver) Listen(ctx context.Context, apm_chan <-chan ApmBucket) (<-chan bool, error) {
  done_ch := make(chan bool)
  self.wait_group.Add(1)
  if _,err := os.Stat(self.conf.TimeseriesDir()); err != nil { return nil, err }

  go self.dumpToCvsFile(ctx, apm_chan)

  go func() {
    defer close(done_ch)
    self.wait_group.Wait()
    if self.stats.err != nil {
      Errorf("Abnormal exit: %v", self.stats.err)
    }
    Debugf("%s", self.stats.String())
  }()
  return done_ch, nil
}

