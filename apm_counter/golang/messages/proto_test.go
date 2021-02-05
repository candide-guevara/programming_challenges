package messages

import "testing"
import "time"

import "apm_counter/util"

func TestTimeserieSerializing(t *testing.T) {
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

  _,err = util.WriteProtoWithPrefixedLen(zip_writer, &ts)
  if err != nil { t.Fatalf("%v", err) }
}

