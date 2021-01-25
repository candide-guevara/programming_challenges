package main

import (
  "fmt"
  "encoding/binary"
  "os"
)

func read_kbd_input(filepath string) error {
  //var buf [24]byte
  var ev [16]linux_input_ev
  fobj, err := os.Open(filepath)
  defer fobj.Close()
  if err != nil {
    return err
  }
  if err := binary.Read(fobj, binary.LittleEndian, &ev); err != nil {
    return err
  }
  for _, ev_i := range ev {
    //fmt.Printf("ev=%+v\n",   ev_i)
    fmt.Printf("ev=%s\n", ev_i.String())
  }
  return nil
}

func read_mouse_input(filepath string) {
}

func main() {
  if err := read_kbd_input("/dev/input/event19"); err != nil {
    fmt.Printf("fiasco : %v\n", err)
  }
  fmt.Println("done")
}

