package messages

import "compress/gzip"
import "fmt"
import "os"
import fpmod "path/filepath"
import "testing"
import "time"
import "google.golang.org/protobuf/proto"
import timepb "google.golang.org/protobuf/types/known/timestamppb"

func writeAndClose(file *os.File, zip_writer *gzip.Writer, msg proto.Message) error {
  defer zip_writer.Close()
  defer file.Close()
  var err error
  var buf []byte
  buf, err = proto.Marshal(msg)
  if err != nil { return err }
  _, err = zip_writer.Write(buf)
  return err
}

// https://developers.google.com/protocol-buffers/docs/techniques#streaming
func TestTimeserieSerializing(t *testing.T) {
  var err error
  var file *os.File
  filepath := fpmod.Join(os.Getenv("TEMP"),
                         fmt.Sprintf("%d.pb.gz", time.Now().Unix()))
  file, err = os.Create(filepath)
  if err != nil { t.Fatalf("%v", err) }
  zip_writer := gzip.NewWriter(file)

  ts := Timeserie {}
  ts.RefTime = timepb.Now()
  ts.OffsetMillis = []uint32 {0, 1, 2, 3, 4, 5, 6}
  ts.KbdCount = []uint32 {0, 1, 2, 3, 4, 5, 6}
  ts.MseCount = []uint32 {0, 1, 2, 3, 4, 5, 6}
  ts.BtnCount = []uint32 {0, 1, 2, 3, 4, 5, 6}

  err = writeAndClose(file, zip_writer, &ts)
  if err != nil { t.Fatalf("%v", err) }
}

