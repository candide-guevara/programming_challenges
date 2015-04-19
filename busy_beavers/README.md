# Find the busiest beavers

It took the idea for this project from Richard Buckland's [computing lectures at UNSW][1].
The goal is to try to come up with a fixed lenght program that will output the most caracters 
to its stdout. The other condition is that the program must halt at some point. For more
details on the problem check [wikipedia][2].

## Dependencies

* C++11 compiler and standard library
* [Scons][3]

## Getting started

* Compile with **scons --opt**
* To know the different options try **find\_beavers -h**
* To run the test suite just do **find\_beavers -t**

## Directory structure

* bin/ : used to build the project
* src/busybeaver/ : the code to find the busiest beavers
* src/include/ : header files
* tst/ : the project's unit tests 

[1]: https://www.openlearning.com/courses/unsw/computing1/Modules/01/Activities/BusyBeaver
[2]: http://en.wikipedia.org/wiki/Busy_beaver
[3]: http://www.scons.org/

