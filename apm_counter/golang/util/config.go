package util

import "flag"
import "fmt"
import fpmod "path/filepath"
import "os"
import "strings"
import "time"

import "apm_counter/types"

type ConfigImpl struct {
  DevFiles_ []string
  SteamDir_ string
  AoeUserDir_ string
  TimeserieDir_ string
  TimeserieRepoName_ string
  RefTime_ time.Time
  WindowDuration_ time.Duration
  OutputPeriod_ time.Duration
  LogLevel_ types.LogT
}

func (self *ConfigImpl) DevicesToListenTo() []string { return self.DevFiles_ }
func (self *ConfigImpl) TimeserieDir() string { return self.TimeserieDir_ }
func (self *ConfigImpl) StartTime() time.Time { return self.RefTime_ }
func (self *ConfigImpl) WindowsDuration() time.Duration { return self.WindowDuration_ }
func (self *ConfigImpl) OutputPeriod() time.Duration { return self.OutputPeriod_ }
func (self *ConfigImpl) LogLevel() types.LogT { return self.LogLevel_ }
func (self *ConfigImpl) AoeUserDir() string {
  return fpmod.Join(self.SteamDir_, self.AoeUserDir_)
}
func (self *ConfigImpl) TimeserieRepo() string {
  return fpmod.Join(self.TimeserieDir_, self.TimeserieRepoName_)
}

var opt_dev_files string
var opt_steam_dir string
var opt_timeserie_dir string
var opt_period_ms uint
var opt_log_lvl string
func init() {
  flag.StringVar(&opt_dev_files, "dev_files", "", "/dev/input files to listen to")
  flag.StringVar(&opt_steam_dir, "steam_dir", "", "Root for the steam library")
  flag.StringVar(&opt_timeserie_dir, "ts_root", "", "Root for the timeseries files")
  flag.UintVar(&opt_period_ms, "period_ms", 0, "Output period for apm in ms")
  flag.StringVar(&opt_log_lvl, "log_lvl", "", "Verbosity level")
}

func (self *ConfigImpl) String() string {
  return fmt.Sprintf(`
  DevFiles_ = %v
  SteamDir_ = '%v'
  AoeUserDir_ = '%v'
  TimeserieDir_ = '%v'
  TimeserieRepoName_ = '%v'
  RefTime_ = %v
  WindowDuration_ = %v
  OutputPeriod_ = %v
  LogLevel_ = %v
  `,
  self.DevFiles_,
  self.SteamDir_,
  self.AoeUserDir_,
  self.TimeserieDir_,
  self.TimeserieRepoName_,
  self.RefTime_.Format("02/01/2006 15:04:05.000"),
  self.WindowDuration_,
  self.OutputPeriod_,
  self.LogLevel_)
}

func NewTestConfig() *ConfigImpl {
  conf := ConfigImpl {
    DevFiles_:nil,
    SteamDir_:"",
    AoeUserDir_:"",
    TimeserieDir_:os.Getenv("TEMP"),
    TimeserieRepoName_:"timeserie_repo.pb.gz",
    RefTime_:time.Now(),
    WindowDuration_:60 * time.Second,
    OutputPeriod_:500 * time.Millisecond,
    LogLevel_:types.LogInfo,
  }
  if len(conf.TimeserieDir()) < 1 {
    panic("Bad default for timeserie dir")
  }
  return &conf
}

func NewConfigImpl() types.Config {
  if flag.Parsed() == false {
    flag.Parse()
  }
  conf := NewTestConfig()

  var dev_files []string
  for _,f := range strings.Split(opt_dev_files, ",") {
    if len(f) > 0 { dev_files = append(dev_files, f) }
  }
  conf.DevFiles_ = dev_files
  if len(opt_steam_dir) > 0 { conf.SteamDir_ = opt_steam_dir }
  if len(opt_timeserie_dir) > 0 { conf.TimeserieDir_ = opt_timeserie_dir }
  if opt_period_ms > 0 { conf.OutputPeriod_ = time.Duration(opt_period_ms) * time.Millisecond }
  if len(opt_log_lvl) > 0 {
    translate := make(map[string]types.LogT)
    translate["trace"] = types.LogTrace
    translate["debug"] = types.LogDebug
    translate["info"] = types.LogInfo
    translate["warn"] = types.LogWarn
    translate["error"] = types.LogError
    lvl,found := translate[strings.ToLower(opt_log_lvl)]
    if !found {
      flag.Usage()
      Fatalf("Bad flags")
    }
    conf.LogLevel_ = lvl
  }
  return conf
}

