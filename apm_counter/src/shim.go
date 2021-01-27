package main

// #include <linux/input-event-codes.h>
import "C"
import "fmt"
import "time"

const Unknown = "UNKNOWN"
const KeyPress = "KEY_PRESS"
const KeyRelease = "KEY_RELEASE"
const KeyRepeat = "KEY_REPEAT"
const DownMov = "DOWN"
const UpMov = "UP"
const RightMov = "RIGHT"
const LeftMov = "LEFT"
const KeyPressCode = 1
const KeyReleaseCode = 0
const KeyRepeatCode = 2

// see linux/input.h
// see /usr/include/linux/input-event-codes.h
type linux_input_ev struct {
  Secs   int64
  Usecs  int64
  EvType uint16
  EvCode uint16
  Value  int32
}

func (self *linux_input_ev) String() string {
  // time.Format uses a dodgy format string (see https://golang.org/pkg/time/#Time.Format)
  return fmt.Sprintf("{%s  %s  %s  time='%s'}",
                     self.TypeName(), self.CodeName(), self.ValueName(),
                     self.EvTime().Format("15:04:05.000"))
}

func (self *linux_input_ev) EvTime() time.Time {
  return time.Unix(self.Secs, self.Usecs * 1000)
}

func (self *linux_input_ev) TypeName() string {
  switch self.EvType {
    case C.EV_SYN      : return "EV_SYN"
    case C.EV_KEY      : return "EV_KEY"
    case C.EV_REL      : return "EV_REL"
    case C.EV_ABS      : return "EV_ABS"
    case C.EV_MSC      : return "EV_MSC"
    case C.EV_SW       : return "EV_SW"
    case C.EV_LED      : return "EV_LED"
    case C.EV_SND      : return "EV_SND"
    case C.EV_REP      : return "EV_REL"
    case C.EV_FF       : return "EV_FF"
    case C.EV_PWR      : return "EV_PWR"
    case C.EV_FF_STATUS: return "EV_FF_STATUS"
    default: return fmt.Sprintf("%s(%d)", Unknown, self.EvType)
  }
}

func (self *linux_input_ev) CodeName() string {
  switch self.EvType {
    case C.EV_KEY      : return self.codeNameKey()
    case C.EV_MSC      : return self.codeNameMsc()
    case C.EV_REL      : return self.codeNameRel()
    case C.EV_SYN      : return self.codeNameSyn()
    default: return fmt.Sprintf("%s(%d)", Unknown, self.EvCode)
  }
}

func (self *linux_input_ev) ValueName() string {
  switch self.EvType {
    case C.EV_KEY      : return self.valueNameKey()
    case C.EV_REL      : return self.valueNameRel()
    default: return fmt.Sprintf("%s(%d)", Unknown, self.Value)
  }
}

func (self *linux_input_ev) valueNameKey() string {
  switch self.Value {
    case KeyPressCode:   return KeyPress
    case KeyReleaseCode: return KeyRelease
    case KeyRepeatCode:  return KeyRepeat
    default: return fmt.Sprintf("%s(%d)", Unknown, self.Value)
  }
}

func (self *linux_input_ev) valueNameRel() string {
  switch self.EvCode {
    case C.REL_X:
      if self.Value > 0 { return fmt.Sprintf("%s(%d)", RightMov, self.Value)
      } else { return fmt.Sprintf("%s(%d)", LeftMov, self.Value) }
    case C.REL_Y: fallthrough
    case C.REL_HWHEEL: fallthrough
    case C.REL_WHEEL: fallthrough
    case C.REL_WHEEL_HI_RES: fallthrough
    case C.REL_HWHEEL_HI_RES:
      if self.Value > 0 { return fmt.Sprintf("%s(%d)", UpMov, self.Value)
      } else { return fmt.Sprintf("%s(%d)", DownMov, self.Value) }
    default: return fmt.Sprintf("%s(%d)", Unknown, self.Value)
  }
}

func (self *linux_input_ev) codeNameRel() string {
  switch self.EvCode {
    case C.REL_X: return "REL_X"
    case C.REL_Y: return "REL_Y"
    case C.REL_Z: return "REL_Z"
    case C.REL_RX: return "REL_RX"
    case C.REL_RY: return "REL_RY"
    case C.REL_RZ: return "REL_RZ"
    case C.REL_HWHEEL: return "REL_HWHEEL"
    case C.REL_DIAL: return "REL_DIAL"
    case C.REL_WHEEL: return "REL_WHEEL"
    case C.REL_MISC: return "REL_MISC"
    case C.REL_RESERVED: return "REL_RESERVED"
    case C.REL_WHEEL_HI_RES: return "REL_WHEEL_HI_RES"
    case C.REL_HWHEEL_HI_RES: return "REL_HWHEEL_HI_RES"
    default: return fmt.Sprintf("%s(%d)", Unknown, self.EvCode)
  }
}

// grep -E '#define *SYN_' /usr/include/linux/input-event-codes.h \
//   | sed -r 's/.*(SYN_\w+).*/case C.\1: return "\1"/'
func (self *linux_input_ev) codeNameSyn() string {
  switch self.EvCode {
    case C.SYN_REPORT: return "SYN_REPORT"
    case C.SYN_CONFIG: return "SYN_CONFIG"
    case C.SYN_MT_REPORT: return "SYN_MT_REPORT"
    case C.SYN_DROPPED: return "SYN_DROPPED"
    default: return fmt.Sprintf("%s(%d)", Unknown, self.EvCode)
  }
}

// grep -E '#define *KEY_' /usr/include/linux/input-event-codes.h \
//   | sed -r 's/.*(KEY_\w+).*/case C.\1: return "\1"/'
func (self *linux_input_ev) codeNameKey() string {
  switch self.EvCode {
    case C.KEY_RESERVED: return "KEY_RESERVED"
    case C.KEY_ESC: return "KEY_ESC"
    case C.KEY_1: return "KEY_1"
    case C.KEY_2: return "KEY_2"
    case C.KEY_3: return "KEY_3"
    case C.KEY_4: return "KEY_4"
    case C.KEY_5: return "KEY_5"
    case C.KEY_6: return "KEY_6"
    case C.KEY_7: return "KEY_7"
    case C.KEY_8: return "KEY_8"
    case C.KEY_9: return "KEY_9"
    case C.KEY_0: return "KEY_0"
    case C.KEY_MINUS: return "KEY_MINUS"
    case C.KEY_EQUAL: return "KEY_EQUAL"
    case C.KEY_BACKSPACE: return "KEY_BACKSPACE"
    case C.KEY_TAB: return "KEY_TAB"
    case C.KEY_Q: return "KEY_Q"
    case C.KEY_W: return "KEY_W"
    case C.KEY_E: return "KEY_E"
    case C.KEY_R: return "KEY_R"
    case C.KEY_T: return "KEY_T"
    case C.KEY_Y: return "KEY_Y"
    case C.KEY_U: return "KEY_U"
    case C.KEY_I: return "KEY_I"
    case C.KEY_O: return "KEY_O"
    case C.KEY_P: return "KEY_P"
    case C.KEY_LEFTBRACE: return "KEY_LEFTBRACE"
    case C.KEY_RIGHTBRACE: return "KEY_RIGHTBRACE"
    case C.KEY_ENTER: return "KEY_ENTER"
    case C.KEY_LEFTCTRL: return "KEY_LEFTCTRL"
    case C.KEY_A: return "KEY_A"
    case C.KEY_S: return "KEY_S"
    case C.KEY_D: return "KEY_D"
    case C.KEY_F: return "KEY_F"
    case C.KEY_G: return "KEY_G"
    case C.KEY_H: return "KEY_H"
    case C.KEY_J: return "KEY_J"
    case C.KEY_K: return "KEY_K"
    case C.KEY_L: return "KEY_L"
    case C.KEY_SEMICOLON: return "KEY_SEMICOLON"
    case C.KEY_APOSTROPHE: return "KEY_APOSTROPHE"
    case C.KEY_GRAVE: return "KEY_GRAVE"
    case C.KEY_LEFTSHIFT: return "KEY_LEFTSHIFT"
    case C.KEY_BACKSLASH: return "KEY_BACKSLASH"
    case C.KEY_Z: return "KEY_Z"
    case C.KEY_X: return "KEY_X"
    case C.KEY_C: return "KEY_C"
    case C.KEY_V: return "KEY_V"
    case C.KEY_B: return "KEY_B"
    case C.KEY_N: return "KEY_N"
    case C.KEY_M: return "KEY_M"
    case C.KEY_COMMA: return "KEY_COMMA"
    case C.KEY_DOT: return "KEY_DOT"
    case C.KEY_SLASH: return "KEY_SLASH"
    case C.KEY_RIGHTSHIFT: return "KEY_RIGHTSHIFT"
    case C.KEY_KPASTERISK: return "KEY_KPASTERISK"
    case C.KEY_LEFTALT: return "KEY_LEFTALT"
    case C.KEY_SPACE: return "KEY_SPACE"
    case C.KEY_CAPSLOCK: return "KEY_CAPSLOCK"
    case C.KEY_F1: return "KEY_F1"
    case C.KEY_F2: return "KEY_F2"
    case C.KEY_F3: return "KEY_F3"
    case C.KEY_F4: return "KEY_F4"
    case C.KEY_F5: return "KEY_F5"
    case C.KEY_F6: return "KEY_F6"
    case C.KEY_F7: return "KEY_F7"
    case C.KEY_F8: return "KEY_F8"
    case C.KEY_F9: return "KEY_F9"
    case C.KEY_F10: return "KEY_F10"
    case C.KEY_NUMLOCK: return "KEY_NUMLOCK"
    case C.KEY_SCROLLLOCK: return "KEY_SCROLLLOCK"
    case C.KEY_KP7: return "KEY_KP7"
    case C.KEY_KP8: return "KEY_KP8"
    case C.KEY_KP9: return "KEY_KP9"
    case C.KEY_KPMINUS: return "KEY_KPMINUS"
    case C.KEY_KP4: return "KEY_KP4"
    case C.KEY_KP5: return "KEY_KP5"
    case C.KEY_KP6: return "KEY_KP6"
    case C.KEY_KPPLUS: return "KEY_KPPLUS"
    case C.KEY_KP1: return "KEY_KP1"
    case C.KEY_KP2: return "KEY_KP2"
    case C.KEY_KP3: return "KEY_KP3"
    case C.KEY_KP0: return "KEY_KP0"
    case C.KEY_KPDOT: return "KEY_KPDOT"
    case C.BTN_LEFT: return "BTN_LEFT"
    case C.BTN_RIGHT: return "BTN_RIGHT"
    case C.BTN_MIDDLE: return "BTN_MIDDLE"
    case C.BTN_SIDE: return "BTN_SIDE" // backward mouse button
    case C.BTN_EXTRA: return "BTN_EXTRA" // forward mouse button
    case C.BTN_FORWARD: return "BTN_FORWARD"
    case C.BTN_BACK: return "BTN_BACK"
    case C.BTN_TASK: return "BTN_TASK"
    default: return fmt.Sprintf("%s(%d)", Unknown, self.EvCode)
  }
}

// grep -E '#define *MSC_' /usr/include/linux/input-event-codes.h \
//   | sed -r 's/.*(MSC_\w+).*/case C.\1: return "\1"/'
func (self *linux_input_ev) codeNameMsc() string {
  switch self.EvCode {
    case C.MSC_SERIAL: return "MSC_SERIAL"
    case C.MSC_PULSELED: return "MSC_PULSELED"
    case C.MSC_GESTURE: return "MSC_GESTURE"
    case C.MSC_RAW: return "MSC_RAW"
    case C.MSC_SCAN: return "MSC_SCAN"
    case C.MSC_TIMESTAMP: return "MSC_TIMESTAMP"
    default: return fmt.Sprintf("%s(%d)", Unknown, self.EvCode)
  }
}

