package main

import "log"

func Fatalf(format string, v ...interface{}) {
  log.Fatalf(format, v...)
}

func Infof(format string, v ...interface{}) {
  log.Printf(format, v...)
}

func Debugf(format string, v ...interface{}) {
  log.Printf(format, v...)
}

