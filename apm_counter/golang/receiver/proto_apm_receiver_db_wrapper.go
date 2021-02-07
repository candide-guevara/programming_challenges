package receiver

import "context"
import "fmt"
import "time"

import "apm_counter/messages"
import "apm_counter/types"
import "apm_counter/util"

type protoApmReceiverDbWrapper struct {
  inner_recv types.ApmReceiver
  conf types.Config
  repo messages.TimeserieRepo
}

func NewProtoApmReceiverDbWrapper(conf types.Config) types.ApmReceiver {
  return &protoApmReceiverDbWrapper {
    NewProtoApmReceiver(conf),
    conf,
    messages.TimeserieRepo{},
  }
}

func (self *protoApmReceiverDbWrapper) Filepath() string {
  return self.inner_recv.(*protoApmReceiver).Filepath()
}

func (self *protoApmReceiverDbWrapper) flushRepo(add_new bool) error {
  var entry *messages.TimeserieRepo_Entry
  if add_new {
    entry = &messages.TimeserieRepo_Entry{
      StartSecs: time.Now().Unix(),
      EndSecs: 0,
      Filepath: self.Filepath(),
    }
    self.repo.Entries = append(self.repo.Entries, entry)
  } else if len(self.repo.Entries) < 1 {
    return fmt.Errorf("Cannot flush if there is no new entry")
  } else {
    entry = self.repo.Entries[len(self.repo.Entries)-1]
    entry.EndSecs = time.Now().Unix()
  }
  return util.WriteProtoIntoZipFile(self.conf.TimeserieRepo(), &self.repo)
}

func (self *protoApmReceiverDbWrapper) Listen(ctx context.Context, apm_chan <-chan types.ApmBucket) (<-chan bool, error) {
  var err error
  var inner_ch <-chan bool
  done_ch := make(chan bool)

  err = util.ReadProtoFromZipFile(self.conf.TimeserieRepo(), &self.repo)
  if err != nil { return nil, err }

  inner_ch, err = self.inner_recv.Listen(ctx, apm_chan)
  if err != nil { return nil, err }

  // This has to be done after Listen so that we know the filepath
  err = self.flushRepo(true)
  if err != nil { return inner_ch, err }

  go func() {
    defer close(done_ch)
    select {
      case <-inner_ch:
        flush_err := self.flushRepo(false)
        if flush_err != nil {
          util.Errorf("Failed final repo flush: %v", flush_err)
        }
    }
  }()
  return done_ch, nil
}

