package receiver

import "fmt"
import "sync"

import "apm_counter/types"

type fileApmReceiverStats struct {
  err error
  filepath string
  filesize int64
}

type baseApmReceiver struct {
  conf types.Config
  stats fileApmReceiverStats
  wait_group *sync.WaitGroup
}

func newBaseApmReceiver(conf types.Config) *baseApmReceiver {
  return &baseApmReceiver {
    conf,
    fileApmReceiverStats{},
    new(sync.WaitGroup),
  }
}

func (self *baseApmReceiver) Filepath() string {
  if len(self.stats.filepath) == 0 { panic("Filepath has not been created yet") }
  return self.stats.filepath
}

func (self *fileApmReceiverStats) String() string {
  return fmt.Sprintf(`
  filepath = '%s'
  filesize = %.2e
  err      = '%v'`,
  self.filepath,
  float32(self.filesize),
  self.err)
}

