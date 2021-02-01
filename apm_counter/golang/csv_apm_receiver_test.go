package main

import "context"
import "fmt"
import "io/ioutil"
import "os"
import "strings"
import "testing"
import "time"

func fillApmsAndExpectedCsv(t *testing.T) (chan ApmBucket, []string) {
  apms := []apmBucketImpl {
    {bucketT{[ActionCnt]uint{0,0,0}}, 10},
    {bucketT{[ActionCnt]uint{3,1,0}}, 20},
    {bucketT{[ActionCnt]uint{2,1,0}}, 30},
    {bucketT{[ActionCnt]uint{2,3,0}}, 40},
  }
  expected_csv := []string {
    fmt.Sprintf("%s,%s,%s", ActionKdb.Name(), ActionMse.Name(), ActionBtn.Name()),
    "0,0,0",
    "3,1,0",
    "2,1,0",
    "2,3,0",
    "",
  }
  ch := make(chan ApmBucket, len(apms))
  // Be careful it is a trap !
  // Randomly segfaults and when it does not then all items in the channel point to the last element in apms
  // for _,a := range apms { ch <- &a }
  for i := range apms { ch <- &apms[i] }
  return ch, expected_csv
}

func compareFileContentToStr(t *testing.T, filepath string, expected_lines []string) {
  content, err := ioutil.ReadFile(filepath)
  if err != nil {
    t.Errorf("Timeseries file could not be read: %v", err)
  }
  content_lines := strings.Split(string(content), "\n")
  t.Logf("Expect: %v", expected_lines)
  t.Logf("Content: %v", content_lines)
  if len(content_lines) != len(expected_lines) {
    t.Errorf("Mismatched lines len")
  }
  for idx,expect := range expected_lines {
    if content_lines[idx] != expect {
      t.Errorf("Line %d content does not match", idx)
    }
  }
}

func csvApmReceiverTestSetup() (context.Context, context.CancelFunc, *ConfigImpl) {
  conf := NewTestConfig()
  conf.timeseries_dir = os.Getenv("TEMP")
  InitLogging(conf)
  ctx, cancel := context.WithCancel(context.Background())
  return ctx, cancel, conf
}

func TestCsvApmRecieverListen(t *testing.T) {
  var err error
  ctx, cancel, conf := apmProviderTestSetup()
  in_apm, expected_csv := fillApmsAndExpectedCsv(t)

  var done_ch <-chan bool
  dumper := NewCsvApmReceiver(conf).(*csvApmReceiver)
  done_ch, err = dumper.Listen(ctx, in_apm)
  if err != nil { t.Fatalf("could not dump to csv: %v", err) }

  time.Sleep(10 * time.Millisecond)
  cancel()
  close(in_apm)
  select {
    case <-done_ch: break
    case <-time.After(10 * time.Millisecond):
      t.Fatalf("Csv receiver could not finish before the timeout")
  }
  compareFileContentToStr(t, dumper.stats.filepath, expected_csv)
  if dumper.stats.err != nil {
    t.Errorf("Csv receiver ended with error: %v", dumper.stats.err)
  }
}

