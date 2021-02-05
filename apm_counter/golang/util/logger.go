package util

import "log"

import "apm_counter/types"

var log_level types.LogT = types.LogFatal

func InitLogging(conf types.Config) {
  log_level = conf.LogLevel()
  log.SetFlags(log.Lmicroseconds | log.Lshortfile)
}

func Fatalf(format string, v ...interface{}) {
  log.SetPrefix("[FATAL] ")
  log.Fatalf(format, v...)
}

func Errorf(format string, v ...interface{}) {
  if log_level <= types.LogError {
    log.SetPrefix("[ERROR] ")
    log.Printf(format, v...)
  }
}

func Warnf(format string, v ...interface{}) {
  if log_level <= types.LogWarn {
    log.SetPrefix("[WARN] ")
    log.Printf(format, v...)
  }
}

func Infof(format string, v ...interface{}) {
  if log_level <= types.LogInfo {
    log.SetPrefix("[INFO] ")
    log.Printf(format, v...)
  }
}

func Debugf(format string, v ...interface{}) {
  if log_level <= types.LogDebug {
  log.SetPrefix("[DEBUG] ")
  log.Printf(format, v...)
}
}

func Tracef(format string, v ...interface{}) {
  if log_level <= types.LogTrace {
    log.SetPrefix("[TRACE] ")
    log.Printf(format, v...)
  }
}

