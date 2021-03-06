package util

import "context"
import "fmt"
import "os"
import "os/signal"
import fpmod "path/filepath"
import "reflect"
import "time"

import "apm_counter/types"

func WaitForClosureReflection(wait_for_millis uint, chs ... interface{}) error {
  Infof("Context done, waiting for channels close")
  timeout := time.NewTimer(time.Duration(wait_for_millis) * time.Millisecond)
  defer timeout.Stop()

  ch_len := len(chs)
  done_count := 0
  cases := make([]reflect.SelectCase, ch_len + 1)
  for idx,ch := range chs {
    cases[idx].Dir = reflect.SelectRecv
    cases[idx].Chan = reflect.ValueOf(ch)
  }
  cases[ch_len].Dir = reflect.SelectRecv
  cases[ch_len].Chan = reflect.ValueOf(timeout.C)

  for done_count < ch_len {
    idx,_,ok := reflect.Select(cases)
    if idx == ch_len {
      fmt.Errorf("Channels (%d/%d) are still open after timeout", done_count, ch_len)
    }
    if !ok {
      done_count += 1
      cases[idx].Chan = reflect.ValueOf(nil)
    }
  }
  return nil
}

func CatchInterruptSignal(ctx context.Context, cancel context.CancelFunc) {
  sig_ch := make(chan os.Signal, 1)
  signal.Notify(sig_ch, os.Interrupt)
  go func() {
    select {
      case <-sig_ch:
        cancel()
        break
      case <-ctx.Done(): break
    }
  }()
}

func ErrOnPrematureClosure(ctx context.Context, read_stx bool) error {
  if !read_stx && ctx.Err() == nil {
    return fmt.Errorf("premature channel closure for consumer")
  }
  return nil
}

func CreateTimeserieFilename(conf types.Config, extension string) string {
  datestr := conf.StartTime().Format("02-01-2006-15-04")
  filepath := fpmod.Join(conf.TimeserieDir(),
                         fmt.Sprintf("timeseries-%s.%s", datestr, extension))
  return filepath
}

func CreateTimeserieFile(conf types.Config, extension string) (*os.File, error) {
  file, err := os.Create(CreateTimeserieFilename(conf, extension))
  return file, err
}

