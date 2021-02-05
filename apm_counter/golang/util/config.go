package util

import "fmt"
import "path/filepath"
import "os"
import "time"

import "apm_counter/types"

type ConfigImpl struct {
  DevFiles_ []string
  SteamDir_ string
  AoeUserDir_ string
  TimeseriesDir_ string
  RefTime_ time.Time
  WindowDuration_ time.Duration
  OutputPeriod_ time.Duration
  LogLevel_ types.LogT
}

func (self *ConfigImpl) DevicesToListenTo() []string { return self.DevFiles_ }
func (self *ConfigImpl) TimeseriesDir() string { return self.TimeseriesDir_ }
func (self *ConfigImpl) StartTime() time.Time { return self.RefTime_ }
func (self *ConfigImpl) WindowsDuration() time.Duration { return self.WindowDuration_ }
func (self *ConfigImpl) OuputPeriod() time.Duration { return self.OutputPeriod_ }
func (self *ConfigImpl) LogLevel() types.LogT { return self.LogLevel_ }
func (self *ConfigImpl) AoeUserDir() string {
  return filepath.Join(self.SteamDir_, self.AoeUserDir_)
}

func (self *ConfigImpl) String() string {
  return fmt.Sprintf(`
  SteamDir_ = '%v'
  AoeUserDir_ = '%v'
  TimeseriesDir_ = '%v'
  RefTime_ = %v
  OutputPeriod_ = %v
  LogLevel_ = %v
  `,
  self.SteamDir_,
  self.AoeUserDir_,
  self.TimeseriesDir_,
  self.RefTime_.Format("02/01/2006 15:04:05.000"),
  self.OutputPeriod_,
  self.LogLevel_)
}

func NewTestConfig() *ConfigImpl {
  conf := ConfigImpl {
    []string {
      "/dev/input/by-id/usb-Logitech_Gaming_Mouse_G502_1393375E3137-event-mouse",
      "/dev/input/by-id/usb-046a_0023-event-kbd",
    },
    "/media/llewelyn_data_b/SteamLibrary",
    "steamapps/compatdata/813780/pfx/drive_c/users/steamuser/Games/Age of Empires 2 DE",
    os.Getenv("TEMP"),
    time.Now(),
    60 * time.Second,
    500 * time.Millisecond,
    types.LogTrace,
  }
  return &conf
}

func NewConfigImpl() types.Config {
  return NewTestConfig()
}

