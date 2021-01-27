package main
// Using /dev/input files to get events directly from kernel
// * Event data format : https://www.kernel.org/doc/html/v4.12/input/input.html

import "context"
import "encoding/binary"
import "fmt"
import "os"
import "sync"
import "time"

type perFileStats struct {
  reads uint
  resizes uint
  raw_ev_count uint
  ev_count uint
  err error
}

func (self perFileStats) String() string {
  return fmt.Sprintf("err='%v'", self.err)
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

func (self *devInputEventProvider) processDevInputEvents(start time.Time, evs []linux_input_ev, out chan EventData) uint {
  for _, ev_i := range evs {
    Infof("ev=%s", ev_i.String())
    out <- EventData{}
  }
  return 0
}

func (self *devInputEventProvider) listenToFile(ctx context.Context, idx int, file *os.File, out chan EventData) {
  const max_len = 512
  const min_len = 4
  const too_short_duration = 50
  const too_long_duration = 100

  defer file.Close()
  defer self.wait_group.Done()
  var all_buf [max_len]linux_input_ev
  var cur_buf []linux_input_ev = all_buf[0:2*min_len]
  var stat perFileStats
  start := self.conf.StartTime()

  for ctx.Err() == nil {
    read_start := time.Now()
    stat.reads += 1
    cur_len := uint(len(cur_buf))
    stat.err = file.SetDeadline(read_start.Add(too_long_duration * time.Millisecond))
    if stat.err != nil { break }
    stat.err = binary.Read(file, binary.LittleEndian, &cur_buf)
    if stat.err != nil { break }
    stat.raw_ev_count += cur_len
    stat.ev_count += self.processDevInputEvents(start, cur_buf, out)

    elapsed_millis := time.Now().Sub(read_start).Milliseconds()
    if elapsed_millis < too_short_duration && cur_len > min_len {
      cur_buf = all_buf[0:cur_len>>1]
      stat.resizes += 1
    }
    if elapsed_millis > too_long_duration && cur_len < max_len {
      cur_buf = all_buf[0:cur_len<<1]
      stat.resizes += 1
    }
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
      Infof("%s = %+v", dev_files[idx], stat)
    }
  }()
  return ch, nil
}

