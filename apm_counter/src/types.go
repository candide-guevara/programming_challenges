package main

/*
Using xlib to intercept X11 events
* https://www.x.org/releases/X11R7.7/doc/libX11/libX11/libX11.html#Event_Handling_Functions
* Example code from `xinput --test-xi2 --root`
  * https://github.com/freedesktop/xorg-xinput/blob/master/src/test_xi2.c
* xlib python bindings example : https://github.com/forsberg/kbdcounter/blob/master/src/xlib.py

Using /dev/input files to get events directly from kernel
* Event data format : https://www.kernel.org/doc/html/v4.12/input/input.html
* 
*/

type EventBatch interface {}

