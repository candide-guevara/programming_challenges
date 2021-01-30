package main

import "log"

var log_level LogT = LogFatal

func InitLogging(conf Config) {
  log_level = conf.LogLevel()
  log.SetFlags(log.Lmicroseconds | log.Lshortfile)
}

func Fatalf(format string, v ...interface{}) {
  log.SetPrefix("[FATAL] ")
  log.Fatalf(format, v...)
}

func Errorf(format string, v ...interface{}) {
  if log_level <= LogError {
    log.SetPrefix("[ERROR] ")
    log.Printf(format, v...)
  }
}

func Warnf(format string, v ...interface{}) {
  if log_level <= LogWarn {
    log.SetPrefix("[WARN] ")
    log.Printf(format, v...)
  }
}

func Infof(format string, v ...interface{}) {
  if log_level <= LogInfo {
    log.SetPrefix("[INFO] ")
    log.Printf(format, v...)
  }
}

func Debugf(format string, v ...interface{}) {
  if log_level <= LogDebug {
  log.SetPrefix("[DEBUG] ")
  log.Printf(format, v...)
}
}

func Tracef(format string, v ...interface{}) {
  if log_level <= LogTrace {
    log.SetPrefix("[TRACE] ")
    log.Printf(format, v...)
  }
}

