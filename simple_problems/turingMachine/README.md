# Turing machine emulator

Emulates a multi-tape turing machine. The number of work tapes can be configured.
There is however only **single** input and output tapes.
You can use as **many different symbols** as there are characters.

## Writing programs for the turing machine

Programs are structured as follows :  
  state (<input symbol|*>, ...) => nextState (<output symbol|*>, ...)(<move tape>, ...)

* There are some special tape symbols : \_ (blank) and * (wildcard)
* To move tape heads use : < (left), > (right), \_ (do not move)

## Getting started

* 2 sample programs are included
* To try them out just run python main.py -p <program>

