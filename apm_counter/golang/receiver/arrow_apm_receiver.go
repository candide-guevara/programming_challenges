// +build fiasco

package receiver

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

import "apm_counter/types"
import "apm_counter/util"

import "github.com/apache/arrow/go/arrow"
import "github.com/apache/arrow/go/arrow/array"
import "github.com/apache/arrow/go/arrow/memory"
import "github.com/apache/arrow/go/arrow/ipc"

const (
  MillisSinceName = "OFF"
  MillisSinceIdx = 3
)

type Appender func(types.ApmBucket)

type arrowApmReceiver struct {
  file *os.File
  writer *ipc.FileWriter
  allocator memory.Allocator
  schema *arrow.Schema
}

func NewArrowApmReceiver(conf types.Config) types.ApmReceiver {
  return &arrowApmReceiver {
    *newBaseApmReceiver(conf),
    nil,
    nil,
    nil,
    nil,
  }
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
    arrow.Field{Name:types.ActionKdb.Name(), Type:arrow.PrimitiveTypes.Uint16},
    arrow.Field{Name:types.ActionMse.Name(), Type:arrow.PrimitiveTypes.Uint16},
    arrow.Field{Name:types.ActionBtn.Name(), Type:arrow.PrimitiveTypes.Uint16},
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
  kbd_builder := record_builder.Field(int(types.ActionKdb)).(*array.Uint16Builder)
  mse_builder := record_builder.Field(int(types.ActionMse)).(*array.Uint16Builder)
  btn_builder := record_builder.Field(int(types.ActionBtn)).(*array.Uint16Builder)
  off_builder := record_builder.Field(MillisSinceIdx).(*array.Uint32Builder)

  appender := func(apm types.ApmBucket) {
    util.Tracef("apm=%v", apm)
    kbd_builder.Append(uint16(apm.Count(types.ActionKdb)))
    mse_builder.Append(uint16(apm.Count(types.ActionMse)))
    btn_builder.Append(uint16(apm.Count(types.ActionBtn)))
    off_builder.Append(uint32(apm.MillisSince()))
  }
  return record_builder, appender
}

func (self *arrowApmReceiver) dumpToArrowFile(ctx context.Context, apm_chan <-chan types.ApmBucket) {
  defer self.wait_group.Done()
  const flush_interval = 16
  flush_counter := 0
  record_builder, appender := self.getRecordBuilderAndAppender()
  defer record_builder.Release()

  loop:for {
    select {
      case apm,ok := <-apm_chan:
        self.stats.err = util.ErrOnPrematureClosure(ctx, ok)
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

func (self *arrowApmReceiver) Listen(ctx context.Context, apm_chan <-chan types.ApmBucket) (<-chan bool, error) {
  done_ch := make(chan bool)
  self.wait_group.Add(1)

  self.file, self.stats.err = util.CreateTimeserieFile(self.conf, "arrow")
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
      util.Errorf("Abnormal exit: %v", self.stats.err)
    }
    self.stats.err = self.writer.Close()
    if self.stats.err == nil {
      self.stats.err = self.file.Close()
    }
    if self.stats.err != nil {
      util.Errorf("Could not close output file: %v", self.stats.err)
    }
    util.Debugf("%s", self.stats.String())
  }()
  return done_ch, nil
}

