# Nurikabe solver

While learning C++ I stumbled upon the [great series][1] by Stephan T. Lavavej.
He challenges you to code a [Nurikabe solver][2] that you can test against his own.
I could not manage to build a faster solver but at least I works :-)

## Dependencies

* A C++11 compiler
* Boost libraries
* [Scons][3]

## Getting started

* Build the code by running scons. Use **--opt** flag to build with optimizations.
* Run a comparison between my implementation and STL's by running **./project**.
* Run the test suite with **./test\_suite**.

## Directory structure

* bin/ : used by scons to store object files
* include/ : header files
* stl\_solution/ : the original unmodified STL solution
* src/ : my nurikabe solver along with a modified STL solution that compiles in my linux box
* src/tst/ : the project's unit tests

[1]:http://channel9.msdn.com/Series/C9-Lectures-Stephan-T-Lavavej-Standard-Template-Library-STL-
[2]:http://en.wikipedia.org/wiki/Nurikabe
[3]:http://www.scons.org/

