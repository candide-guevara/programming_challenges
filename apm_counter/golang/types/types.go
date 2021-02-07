package types

import "context"
import "time"

type ActionT uint
const (
  ActionKdb ActionT = iota
  ActionMse ActionT = iota
  ActionBtn ActionT = iota
  ActionCnt ActionT = iota
)

func (self ActionT) Name() string {
  switch(self) {
    case ActionKdb: return "KBD"
    case ActionMse: return "MSE"
    case ActionBtn: return "BTN"
    default: return "UNKNOWN"
  }
}

type LogT uint
const (
  LogTrace LogT = iota
  LogDebug LogT = iota
  LogInfo  LogT = iota
  LogWarn  LogT = iota
  LogError LogT = iota
  LogFatal LogT = iota
)

type ApmBucket interface {
  MillisSince() uint
  Count(action ActionT) uint
}

type SingleAction struct {
  MillisSince_ uint
  ActionCode_ ActionT
}

func (self *SingleAction) MillisSince() uint { return self.MillisSince_ }
func (self *SingleAction) DurationSince() time.Duration {
  return time.Duration(self.MillisSince_) * time.Millisecond
}
func (self *SingleAction) ActionCode() ActionT { return self.ActionCode_ }

type EventProvider interface {
  Listen(ctx context.Context) (<-chan SingleAction, error)
}

type ApmProvider interface {
  AggregateEvents(ctx context.Context, ev_chan <-chan SingleAction) (<-chan ApmBucket, error)
}

type ApmReceiver interface {
  Listen(ctx context.Context, apm_chan <-chan ApmBucket) (<-chan bool, error)
}

type TimeseriesDb interface {
  RecordTimeseries()
  GetTimeserieFilepath()
}

type Config interface {
  DevicesToListenTo() []string
  TimeseriesDir() string
  AoeUserDir() string
  StartTime() time.Time
  WindowsDuration() time.Duration
  OutputPeriod() time.Duration
  LogLevel() LogT
  String() string
}

