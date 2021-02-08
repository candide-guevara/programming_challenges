package util

import "flag"
import "reflect"
import "testing"
import "time"

import "apm_counter/types"

func TestNewConfigImpl(t *testing.T) {
  cmd_line := []string{
    "--dev_files=coco,loco,",
    "--steam_dir=some_dir",
    "--ts_root=some_other_dir",
    "--period_ms=666",
    "--log_lvl=WARN",
  }
  flag.CommandLine.Parse(cmd_line)
  conf := NewConfigImpl()
  expect_dev_files := []string{"coco", "loco"}
  parse_ok := reflect.DeepEqual(expect_dev_files, conf.DevicesToListenTo()) &&
              (conf.OutputPeriod() == 666 * time.Millisecond) &&
              (conf.TimeserieDir() == "some_other_dir") &&
              (conf.LogLevel() == types.LogWarn)
  if !parse_ok {
    t.Errorf("Error parsing:\n%v\n%v", cmd_line, conf)
  }
}

