package messages

import "io"
import "errors"
import "time"
import "testing"

import "apm_counter/util"
import "google.golang.org/protobuf/proto"

func TestTimeserieSerializing(t *testing.T) {
  const write_cycles = 3
  var err error
  var zip_writer *util.FileZipWriter
  conf := util.NewTestConfig()
  filepath := util.CreateTimeserieFilename(conf, "pb.gz")
  zip_writer, err = util.NewFileZipWriter(filepath)
  if err != nil { t.Fatalf("%v", err) }

  ts := Timeserie {}
  ts.Metadata = &Timeserie_Metadata { RefSecs:time.Now().Unix(), RefNanos:0 }
  ts.OffsetMillis = []uint32 {0, 1, 2, 3, 4, 5, 6}
  ts.KbdCount = []uint32 {0, 1, 2, 3, 4, 5, 6}
  ts.MseCount = []uint32 {0, 1, 2, 3, 4, 5, 6}
  ts.BtnCount = []uint32 {0, 1, 2, 3, 4, 5, 6}

  for i:=0; i<write_cycles; i++ {
    var wrote int
    wrote,err = util.WriteProtoWithPrefixedLen(zip_writer, &ts)
    t.Logf("cycle=%d, wrote=%d, pb_size=%d", i, wrote, proto.Size(&ts))
    if err != nil { t.Fatalf("cycle=%d, %v", i, err) }
  }
  err = zip_writer.Close()
  if err != nil { t.Fatalf("%v", err) }

  var zip_reader *util.FileZipReader
  zip_reader, err = util.NewFileZipReader(filepath)
  if err != nil { t.Fatalf("%v", err) }
  for i:=0; i<write_cycles; i++ {
    var read int
    read_ts := Timeserie {}
    read,err = util.ReadProtoWithPrefixedLen(zip_reader, &read_ts)
    if err != nil {
      if !errors.Is(err, io.EOF) { t.Fatalf("cycle=%d, %v", i, err) }
    }
    if !proto.Equal(&read_ts, &ts) {
      t.Errorf("cycle=%d, read=%d\n%v != %v", i, read, &read_ts, &ts)
    }
  }
}

