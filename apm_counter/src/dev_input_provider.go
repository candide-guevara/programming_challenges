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
  resizes uint
}

func (self *SingleAction) String() string {
  return ""
}

func (self *perFileStats) String() string {
  return fmt.Sprintf("ev_count = %d\nbytes_read = %.2e\nreads = %.2e\nresizes = %d\nerr='%v'",
                     self.ev_count, float32(self.bytes_read), float32(self.reads), self.resizes, self.err)
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
      is_same_stroke := (data.MillisSince() - self.lastStrokeEvMillis < between_strokes_millis)
      self.lastStrokeEvMillis = data.MillisSince()
      if is_same_stroke { continue }
    }
    count += 1
    select {
      case out <-data: // Tracef("data=%v", data)
      case <-ctx.Done(): break loop
    }
  }
  return count
}

func (self *devInputEventProvider) listenToFile(ctx context.Context, idx int, file *os.File, out chan<- SingleAction) {
  // We use a base-6 because a single keystroke is composed of 6 individual events
  const max_len = linuxInputEvSize * 96
  const min_len = linuxInputEvSize * 6
  const read_timeout = 50 * time.Millisecond

  defer file.Close()
  defer self.wait_group.Done()

  var all_buf [max_len]byte
  var cur_buf []byte = all_buf[0:min_len]
  var stat perFileStats
  var actual_len int

  loop:for ctx.Err() == nil {
    cur_len := uint(len(cur_buf))
    read_start := time.Now()
    stat.err = file.SetDeadline(read_start.Add(read_timeout))
    if stat.err != nil { break loop }
    actual_len, stat.err = file.Read(cur_buf)
    if stat.err != nil {
      if !errors.Is(stat.err, os.ErrDeadlineExceeded) { break loop }
      stat.err = nil
      if cur_len > min_len {
        cur_buf = all_buf[0:cur_len>>1]
        stat.resizes += 1
        Tracef("Decrease buf size=%d -> %d, read=%d", cur_len, len(cur_buf), actual_len)
      }
    }

    stat.ev_count += self.processDevInputEvents(ctx, all_buf[0:actual_len], out)
    elapsed := time.Since(read_start)

    if cur_len < max_len && elapsed < (read_timeout/2) {
      cur_buf = all_buf[0:cur_len<<1]
      stat.resizes += 1
      Tracef("Increase buf size=%d -> %d, read=%d", cur_len, len(cur_buf), actual_len)
    }
    stat.reads += 1
    stat.bytes_read += uint(actual_len)
  }
  self.stats[idx] = stat
}

func (self *devInputEventProvider) Listen(ctx context.Context) (<-chan SingleAction, error) {
  dev_files := self.conf.DevicesToListenTo()
  ch := make(chan SingleAction, 8)

  for idx,filepath := range dev_files {
    fobj, err := os.Open(filepath)
    if err != nil { return nil, err }
    self.wait_group.Add(1)
    go self.listenToFile(ctx, idx, fobj, ch)
  }

  go func() {
    self.wait_group.Wait()
    close(ch)
    for idx, stat := range self.stats {
      Debugf("%s :\n%v", dev_files[idx], &stat)
    }
  }()
  return ch, nil
}

