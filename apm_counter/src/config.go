package main

import "time"

type ConfigImpl struct {
  dev_files []string
  start time.Time
  output_period uint
}

func (self *ConfigImpl) DevicesToListenTo() []string { return self.dev_files }
func (self *ConfigImpl) StartTime() time.Time { return self.start }
func (self *ConfigImpl) OuputPeriodMillis() uint { return self.output_period }

func NewConfigImpl() Config {
  conf := ConfigImpl {
    []string {
      "/dev/input/by-id/usb-Logitech_Gaming_Mouse_G502_1393375E3137-event-mouse",
      "/dev/input/by-id/usb-046a_0023-event-kbd",
    },
    time.Now(),
    500,
  }
  return &conf
}

