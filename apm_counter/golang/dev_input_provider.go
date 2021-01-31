package main
// Using /dev/input files to get events directly from kernel
// * Event data format : https://www.kernel.org/doc/html/v4.12/input/input.html

import "bytes"
import "context"
import "encoding/binary"
import "errors"
import "fmt"
import "io"
import "os"
import "sync"
import "time"

type perFileStats struct {
  err error
  ev_count uint
  bytes_read uint
  reads uint
  read_tos uint
}

func (self *perFileStats) String() string {
  return fmt.Sprintf(`
  ev_count   = %d
  bytes_read = %.2e
  reads      = %.2e
  read_tos   = %.2e
  err        = '%v'`,
  self.ev_count,
  float32(self.bytes_read),
  float32(self.reads),
  float32(self.read_tos),
  self.err)
}

type devInputEventProvider struct {
  conf Config
  ev_time_ref time.Time
  lastStrokeEvMillis uint
  wait_group *sync.WaitGroup
  stats []perFileStats
}

func NewDevInputEventProvider(conf Config) EventProvider {
  o := devInputEventProvider{
    conf,
    conf.StartTime(),
    0,
    new(sync.WaitGroup),
    make([]perFileStats, len(conf.DevicesToListenTo())),
  }
  return &o
}

func (self *devInputEventProvider) linuxEvToSingleAction(ev *linuxInputEv) (SingleAction, bool) {
  var action ActionT
  if ev.IsMseClick() {
    action = ActionBtn
  } else if ev.IsMseStroke() {
    action = ActionMse
  } else if ev.IsAnyKeyPress() && !ev.IsModifierKey() {
    action = ActionKdb
  } else {
    return SingleAction{}, false
  }
  return SingleAction {
    uint(ev.EvTime().Sub(self.ev_time_ref).Milliseconds()),
    action,
  }, true
}

func (self *devInputEventProvider) processDevInputEvents(ctx context.Context, buf []byte, out chan<- SingleAction) uint {
  const between_strokes_millis = 67
  var ev linuxInputEv
  var count uint = 0
  stream := bytes.NewReader(buf)

  loop:for {
    err := binary.Read(stream, binary.LittleEndian, &ev)
    if err != nil {
      if !errors.Is(err, io.EOF) { panic("marshalling should not fail") }
      break loop
    }
    //Tracef("ev=%s", ev.String())
    data, valid := self.linuxEvToSingleAction(&ev)
    if !valid { continue }
    if data.ActionCode() == ActionMse {
      is_same_stroke := (data.MillisSince() - self.lastStrokeEvMillis) < between_strokes_millis
      self.lastStrokeEvMillis = data.MillisSince()
      if is_same_stroke { continue }
    }
    count += 1
    select {
      case out <-data: //Tracef("ref_time=%v, data=%v", self.ev_time_ref, data)
      case <-ctx.Done(): break loop
    }
  }
  return count
}

func (self *devInputEventProvider) listenToFile(ctx context.Context, idx int, filepath string, out chan<- SingleAction) {
  const read_timeout = 50 * time.Millisecond
  defer self.wait_group.Done()

  // A single keystroke is composed of 6 individual events
  var file *os.File
  var stat perFileStats
  var actual_len int
  byte_buffer := make([]byte, linuxInputEvSize * 128)
  read_end := time.Now().Add(read_timeout)

  read_tick := time.NewTicker(read_timeout)
  defer read_tick.Stop()

  file, stat.err = os.Open(filepath)
  if stat.err != nil { return }
  defer file.Close()

  loop:for ctx.Err() == nil {
    stat.reads += 1
    stat.err = file.SetDeadline(read_end)
    if stat.err != nil { break loop }

    actual_len, stat.err = file.Read(byte_buffer)
    if stat.err != nil {
      if !errors.Is(stat.err, os.ErrDeadlineExceeded) { break loop }
      stat.err = nil
      stat.read_tos += 1
      read_end = read_end.Add(read_timeout)
      continue
    }

    stat.ev_count += self.processDevInputEvents(ctx, byte_buffer[0:actual_len], out)
    stat.bytes_read += uint(actual_len)
    if actual_len == len(byte_buffer) { continue }

    select {
      case t := <-read_tick.C:
        if read_end.After(t.Add(read_timeout)) { break }
        read_end = t.Add(read_timeout)
      case <-ctx.Done(): break loop
    }
  }
  self.stats[idx] = stat
}

func (self *devInputEventProvider) Listen(ctx context.Context) (<-chan SingleAction, error) {
  dev_files := self.conf.DevicesToListenTo()
  ch := make(chan SingleAction, 8)

  for idx,filepath := range dev_files {
    // check the file exists first
    if _,err := os.Stat(filepath); err != nil { return nil, err }
    self.wait_group.Add(1)
    go self.listenToFile(ctx, idx, filepath, ch)
  }

  go func() {
    defer close(ch)
    self.wait_group.Wait()
    for idx, stat := range self.stats {
      if stat.err != nil {
        Errorf("Abnormal exit: %v", stat.err)
      }
      Debugf("%s:%v", dev_files[idx], &stat)
    }
  }()
  return ch, nil
}

