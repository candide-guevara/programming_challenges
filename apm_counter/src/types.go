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
  Time() time.Time
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

type Config interface {
  StartTime() time.Time
  DevicesToListenTo() []string
  OuputPeriodMillis() uint
  LogLevel() LogT
}

