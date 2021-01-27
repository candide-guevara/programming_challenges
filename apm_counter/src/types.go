package main

import "context"
import "time"

const (
  KdbAction = iota
  MseAction = iota
  BtnAction = iota
)

type EventData struct {
  millis_offset uint
  action_code uint
}

type EventReader interface {
  TimeOffset() uint
  ActionCode() uint
}

type EventProvider interface {
  Listen(ctx context.Context) (chan EventData, error)
}

type Config interface {
  StartTime() time.Time
  DevicesToListenTo() []string
  OuputPeriodMillis() uint
}

