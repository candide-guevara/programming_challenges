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

func (self *perFileStats) String() string {
  return fmt.Sprintf("ev_count = %d\nbytes_read = %d\nreads = %d\nresizes = %d\nerr='%v'",
                     self.ev_count, self.bytes_read, self.reads, self.resizes, self.err)
}

type devInputEventProvider struct {
  conf Config
  wait_group *sync.WaitGroup
  stats []perFileStats
}

func NewDevInputEventProvider(conf Config) EventProvider {
  o := devInputEventProvider{
    conf,
    new(sync.WaitGroup),
    make([]perFileStats, len(conf.DevicesToListenTo())),
  }
  return &o
}

func (self *devInputEventProvider) processDevInputEvents(ctx context.Context, start time.Time, buf []byte, out chan EventData) uint {
  var ev linux_input_ev
  stream := bytes.NewReader(buf)
  for {
    err := binary.Read(stream, binary.LittleEndian, &ev)
    if err != nil {
      if !errors.Is(err, io.EOF) { panic("marshalling should not fail") }
      break
    }
    Debugf("ev=%s", ev.String())
    select {
      case out <- EventData{}:
      case <- ctx.Done(): break
    }
  }
  return (uint(len(buf)) / linuxInputEvSize)
}

func (self *devInputEventProvider) listenToFile(ctx context.Context, idx int, file *os.File, out chan EventData) {
  // We use a base-6 because a single keystroke is composed of 6 individual events
  const max_len = linuxInputEvSize * 96
  const min_len = linuxInputEvSize * 6
  const timeout_millis = 50 * time.Millisecond

  defer file.Close()
  defer self.wait_group.Done()

  var all_buf [max_len]byte
  var cur_buf []byte = all_buf[0:2*min_len]
  var start time.Time = self.conf.StartTime()
  var stat perFileStats
  var actual_len int

  for ctx.Err() == nil {
    cur_len := uint(len(cur_buf))
    read_start := time.Now()
    stat.err = file.SetDeadline(read_start.Add(timeout_millis))
    if stat.err != nil { break }
    actual_len, stat.err = file.Read(cur_buf)
    if stat.err != nil {
      if !errors.Is(stat.err, os.ErrDeadlineExceeded) { break }
      stat.err = nil
      if cur_len < max_len {
        cur_buf = all_buf[0:cur_len<<1]
        stat.resizes += 1
      }
    }

    stat.ev_count += self.processDevInputEvents(ctx, start, all_buf[0:actual_len], out)
    elapsed_millis := time.Now().Sub(read_start)

    if cur_len > min_len && elapsed_millis < (timeout_millis/2) {
      cur_buf = all_buf[0:cur_len>>1]
      stat.resizes += 1
    }
    stat.reads += 1
    stat.bytes_read += uint(actual_len)
  }
  self.stats[idx] = stat
}

func (self *devInputEventProvider) Listen(ctx context.Context) (chan EventData, error) {
  dev_files := self.conf.DevicesToListenTo()
  ch := make(chan EventData, 8)

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
      Debugf("%s = %v", dev_files[idx], &stat)
    }
  }()
  return ch, nil
}

