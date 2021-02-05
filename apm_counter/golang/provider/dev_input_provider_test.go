package provider

import "context"
import "encoding/binary"
import "fmt"
import "math/rand"
import "path/filepath"
import "os"
import "syscall"
import "time"
import "testing"

import "apm_counter/types"
import "apm_counter/util"

func collectActionsFrom(expected_len int, in_ch <-chan types.SingleAction) <-chan []types.SingleAction {
  ch := make(chan []types.SingleAction)
  go func() {
    var actions []types.SingleAction
    for a := range in_ch {
      actions = append(actions, a)
      if len(actions) == expected_len { break }
    }
    ch <- actions
    close(ch)
  }()
  return ch
}

func pushInputData(t *testing.T, conf types.Config, pipe_path string) ([]types.SingleAction, <-chan bool) {
  ch := make(chan bool)
  // Dummy config has rounded the date to seconds for simplicity
  dummy_time := conf.StartTime().Unix()
  events := []linuxInputEv{
   {dummy_time + 0, 0, EV_KEY, KEY_1, KeyPressCode},
   {dummy_time + 1, 1, EV_KEY, KEY_1, KeyReleaseCode},
   {dummy_time + 2, 2, EV_REL, REL_X, 0},
   {dummy_time + 2, 2000, EV_REL, REL_X, 0},
   {dummy_time + 3, 3, EV_KEY, BTN_LEFT, KeyPressCode},
   {dummy_time + 4, 4, EV_KEY, BTN_LEFT, KeyReleaseCode},
   {dummy_time + 5, 5, EV_REL, REL_X, 0},
  }
  expected_actions := []types.SingleAction{
    {0, types.ActionKdb},
    {2000, types.ActionMse},
    {3000, types.ActionBtn},
    {5000, types.ActionMse},
  }
  go func() {
    file, err := os.OpenFile(pipe_path, os.O_WRONLY, 0666)
    if err != nil {
      t.Fatalf("Failed to open pipe: %v", err)
    }
    for _,ev := range events {
      err = binary.Write(file, binary.LittleEndian, &ev)
      if err != nil { t.Fatalf("failed to write input: %v", err) }
    }
    close(ch)
  }()
  return expected_actions, ch
}

func DevInputProviderTestSetup(pipe_path string) (context.Context, context.CancelFunc, types.Config) {
  conf := util.NewTestConfig()
  conf.DevFiles_ = make([]string, 1)
  conf.DevFiles_[0] = pipe_path
  conf.RefTime_ = conf.StartTime().Truncate(time.Second)
  util.InitLogging(conf)
  ctx, cancel := context.WithCancel(context.Background())
  return ctx, cancel, conf
}

func createNamedPipe(t *testing.T) string {
  var err error
  rand.Seed(time.Now().UnixNano())
  pipe_path := filepath.Join(os.Getenv("TEMP"),
                             fmt.Sprintf("pipe-%d", rand.Int63()))
  err = syscall.Mkfifo(pipe_path, 0666)
	if err != nil {
		t.Fatalf("Failed to create pipe: %v", err)
	}
  return pipe_path
}

func compareActions(t *testing.T, expected_actions []types.SingleAction, actions []types.SingleAction) {
  if len(expected_actions) != len(actions) {
    t.Errorf("mismatched action count")
  }
  t.Logf("actions: %v", actions)
  t.Logf("expected_actions: %v", expected_actions)
  for idx,expect := range expected_actions {
    if expect.MillisSince() != actions[idx].MillisSince() {
      t.Errorf("mismatched action millis since")
    }
    if expect.ActionCode() != actions[idx].ActionCode() {
      t.Errorf("mismatched action code")
    }
  }
}

func TestDevInputProviderListen(t *testing.T) {
  var err error
  pipe_path := createNamedPipe(t)
  defer os.Remove(pipe_path)
  ctx, cancel, conf := DevInputProviderTestSetup(pipe_path)

  var in_ev <-chan types.SingleAction
  prov := NewDevInputEventProvider(conf).(*devInputEventProvider)
  in_ev, err = prov.Listen(ctx)
  if err != nil { t.Fatalf("could not listen: %v", err) }

  expected_actions, done_ch := pushInputData(t, conf, pipe_path)
  out_ev := collectActionsFrom(len(expected_actions), in_ev)

  select {
    case <-done_ch: break
    case <-time.After(100 * time.Millisecond):
      t.Fatalf("Failed to input before timeout")
  }
  select {
    case actions := <-out_ev:
      cancel()
      compareActions(t, expected_actions, actions)
    case <-time.After(100 * time.Millisecond):
      t.Fatalf("Failed to collect enough before timeout")
  }
  select {
    case _,ok := <-in_ev:
      if ok { t.Fatalf("Did not consume all events in the pipe") }
    case <-time.After(100 * time.Millisecond):
      t.Fatalf("Failed to close dev input channel before timeout")
  }
  if prov.stats[0].err != nil {
    t.Errorf("Dev input ended with error: %v", prov.stats[0].err)
  }
}

