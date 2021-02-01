package main

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
  millis_since uint
  action_code ActionT
}

func (self *SingleAction) MillisSince() uint { return self.millis_since }
func (self *SingleAction) DurationSince() time.Duration {
  return time.Duration(self.millis_since) * time.Millisecond
}
func (self *SingleAction) ActionCode() ActionT { return self.action_code }

type EventProvider interface {
  Listen(ctx context.Context) (<-chan SingleAction, error)
}

type ApmProvider interface {
  AggregateEvents(ctx context.Context, ev_chan <-chan SingleAction) (<-chan ApmBucket, error)
}

type ApmReceiver interface {
  Listen(ctx context.Context, apm_chan <-chan ApmBucket) (<-chan bool, error)
}

type Config interface {
  DevicesToListenTo() []string
  TimeseriesDir() string
  AoeUserDir() string
  StartTime() time.Time
  WindowsDuration() time.Duration
  OuputPeriod() time.Duration
  LogLevel() LogT
  String() string
}

