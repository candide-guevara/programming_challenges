package main

/*
#cgo CFLAGS: -O0 -Wno-incompatible-pointer-types

#include <linux/input.h>
// Conflicts with definition of struct timeval in linux/input.h
// #include <linux/time.h>
#include <stdio.h>
#include <stdint.h>

// dude just use `lscpu | grep Endian`
void get_endianness() {
  uint32_t x = 1;
  char *p = &x;
  if (*p) printf("little endian\n");
  else    printf("big endian\n");
}

void get_input_event_layout() {
  printf("sizeof(struct input_event)=%d\n", sizeof(struct input_event));  
  printf("sizeof(struct timeval)=%d\n", sizeof(struct timeval));  
  printf("sizeof(__kernel_ulong_t)=%d\n", sizeof(__kernel_ulong_t));  
  printf("sizeof(__kernel_old_time_t)=%d\n", sizeof(__kernel_old_time_t));  
  printf("sizeof(__kernel_suseconds_t)=%d\n", sizeof(__kernel_suseconds_t));  
  printf("sizeof(time_t)=%d\n", sizeof(time_t));  
}
*/
import "C"

func main() {
  C.get_endianness()
  C.get_input_event_layout()
}

