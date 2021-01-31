package main

import "fmt"
import "path/filepath"
import "os"
import "time"

type ConfigImpl struct {
  dev_files []string
  steam_dir string
  aoe_user_dir string
  timeseries_dir string
  ref_time time.Time
  output_period uint
  log_level LogT
}

func (self *ConfigImpl) DevicesToListenTo() []string { return self.dev_files }
func (self *ConfigImpl) TimeseriesDir() string { return self.timeseries_dir }
func (self *ConfigImpl) StartTime() time.Time { return self.ref_time }
func (self *ConfigImpl) OuputPeriodMillis() uint { return self.output_period }
func (self *ConfigImpl) LogLevel() LogT { return self.log_level }
func (self *ConfigImpl) AoeUserDir() string {
  return filepath.Join(self.steam_dir, self.aoe_user_dir)
}

func (self *ConfigImpl) String() string {
  return fmt.Sprintf(`
  steam_dir = '%v'
  aoe_user_dir = '%v'
  timeseries_dir = '%v'
  ref_time = %v
  output_period = %v
  log_level = %v
  `,
  self.steam_dir,
  self.aoe_user_dir,
  self.timeseries_dir,
  self.ref_time.Format("02/01/2006 15:04:05.000"),
  self.output_period,
  self.log_level)
}

func NewConfigImpl() Config {
  conf := ConfigImpl {
    []string {
      "/dev/input/by-id/usb-Logitech_Gaming_Mouse_G502_1393375E3137-event-mouse",
      "/dev/input/by-id/usb-046a_0023-event-kbd",
    },
    "/media/llewelyn_data_b/SteamLibrary",
    "steamapps/compatdata/813780/pfx/drive_c/users/steamuser/Games/Age of Empires 2 DE",
    os.Getenv("TEMP"),
    time.Now(),
    500,
    LogTrace,
  }
  return &conf
}

