// +build fiasco

package fiasco

// Not the best format to persist the data
// * Arrow is not parquet (see https://arrow.apache.org/faq)
//   * Not easy to compress, not directly interoperable with pandas
//   * Metadata limited to key-value
// * The golang API is horrible
//   * Cast to explicit types for the builders
//   * Lots of defers for different objects

import "context"
import "fmt"
import "io"
import "os"
import fpmod "path/filepath"
import "sync"
/*
import "github.com/apache/arrow/go/arrow"
import "github.com/apache/arrow/go/arrow/array"
import "github.com/apache/arrow/go/arrow/memory"
import "github.com/apache/arrow/go/arrow/ipc"
*/
const (
  MillisSinceName = "OFF"
  MillisSinceIdx = 3
)

type Appender func(ApmBucket)

type arrowApmReceiverStats struct {
  err error
  filepath string
  filesize int64
}

type arrowApmReceiver struct {
  conf Config
  file *os.File
  writer *ipc.FileWriter
  allocator memory.Allocator
  schema *arrow.Schema
  stats arrowApmReceiverStats
  wait_group *sync.WaitGroup
}

func NewArrowApmReceiver(conf Config) ApmReceiver {
  return &arrowApmReceiver {
    conf,
    nil,
    nil,
    nil,
    nil,
    arrowApmReceiverStats{},
    new(sync.WaitGroup),
  }
}

func (self *arrowApmReceiverStats) String() string {
  return fmt.Sprintf(`
  filepath = '%s'
  filesize = %.2e
  err      = '%v'`,
  self.filepath,
  float32(self.filesize),
  self.err)
}

func (self *arrowApmReceiver) saveToDisk(record array.Record) (int64, error) {
  defer record.Release()
  self.stats.err = self.writer.Write(record)
  if self.stats.err != nil {
    return 0, self.stats.err
  }
  return self.file.Seek(0, io.SeekCurrent)
}

func (self *arrowApmReceiver) getDataSchema() *arrow.Schema {
  cols := []arrow.Field {
    arrow.Field{Name:ActionKdb.Name(), Type:arrow.PrimitiveTypes.Uint16},
    arrow.Field{Name:ActionMse.Name(), Type:arrow.PrimitiveTypes.Uint16},
    arrow.Field{Name:ActionBtn.Name(), Type:arrow.PrimitiveTypes.Uint16},
    arrow.Field{Name:MillisSinceName,  Type:arrow.PrimitiveTypes.Uint32},
  }
  ref_unix_secs := self.conf.StartTime().Unix()
  metadata := arrow.NewMetadata(
    []string{MillisSinceName},
    []string{fmt.Sprintf("%d", ref_unix_secs)},
  )
  schema := arrow.NewSchema(cols, &metadata)
  //schema := arrow.NewSchema(cols, nil)
  return schema
}

func (self *arrowApmReceiver) getRecordBuilderAndAppender() (*array.RecordBuilder, Appender) {
  record_builder := array.NewRecordBuilder(self.allocator, self.schema)
  kbd_builder := record_builder.Field(int(ActionKdb)).(*array.Uint16Builder)
  mse_builder := record_builder.Field(int(ActionMse)).(*array.Uint16Builder)
  btn_builder := record_builder.Field(int(ActionBtn)).(*array.Uint16Builder)
  off_builder := record_builder.Field(MillisSinceIdx).(*array.Uint32Builder)

  appender := func(apm ApmBucket) {
    Tracef("apm=%v", apm)
    kbd_builder.Append(uint16(apm.Count(ActionKdb)))
    mse_builder.Append(uint16(apm.Count(ActionMse)))
    btn_builder.Append(uint16(apm.Count(ActionBtn)))
    off_builder.Append(uint32(apm.MillisSince()))
  }
  return record_builder, appender
}

func (self *arrowApmReceiver) createNewArrowFile() (*os.File, error) {
  datestr := self.conf.StartTime().Format("02-01-2006-15-04")
  filepath := fpmod.Join(self.conf.TimeseriesDir(),
                         fmt.Sprintf("timeseries-%s.arrow", datestr))
  file, err := os.Create(filepath)
  return file, err
}

func (self *arrowApmReceiver) dumpToArrowFile(ctx context.Context, apm_chan <-chan ApmBucket) {
  defer self.wait_group.Done()
  const flush_interval = 16
  flush_counter := 0
  record_builder, appender := self.getRecordBuilderAndAppender()
  defer record_builder.Release()

  loop:for {
    select {
      case apm,ok := <-apm_chan:
        self.stats.err = ErrOnPrematureClosure(ctx, ok)
        if !ok { break loop }
        appender(apm)
        flush_counter += 1
        if flush_counter % flush_interval == 0 {
          self.stats.filesize, self.stats.err = self.saveToDisk(record_builder.NewRecord())
          if self.stats.err != nil { break loop }
        }
      case <-ctx.Done(): break loop
    }
  }
  if (self.stats.err == nil) && (flush_counter % flush_interval != 0) {
    self.stats.filesize, self.stats.err = self.saveToDisk(record_builder.NewRecord())
  }
}

func (self *arrowApmReceiver) Listen(ctx context.Context, apm_chan <-chan ApmBucket) (<-chan bool, error) {
  done_ch := make(chan bool)
  self.wait_group.Add(1)

  self.file, self.stats.err = self.createNewArrowFile()
  if self.stats.err != nil { return nil, self.stats.err }
  self.allocator = memory.NewGoAllocator()
  self.schema = self.getDataSchema()
  self.stats.filepath = self.file.Name()
  self.writer, self.stats.err = ipc.NewFileWriter(self.file,
                                                  ipc.WithSchema(self.schema),
                                                  ipc.WithAllocator(self.allocator))
  if self.stats.err != nil { return nil, self.stats.err }

  go self.dumpToArrowFile(ctx, apm_chan)

  go func() {
    defer close(done_ch)
    self.wait_group.Wait()
    if self.stats.err != nil {
      Errorf("Abnormal exit: %v", self.stats.err)
    }
    self.stats.err = self.writer.Close()
    if self.stats.err == nil {
      self.stats.err = self.file.Close()
    }
    if self.stats.err != nil {
      Errorf("Could not close output file: %v", self.stats.err)
    }
    Debugf("%s", self.stats.String())
  }()
  return done_ch, nil
}

