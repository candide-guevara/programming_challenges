# Graph traversal algorithms

Silly implementation of constant time graph traversals.
Instead of keeping the visited nodes inside a separate stack (depth-first) or queue (breadth-first) the algorithms will overwrite the graph data itself as it is traversed.

## Algorithms

* Destructive depth first : rewrites the edges of the graph to create a linked list that tracks the branch that is being traversed.
* Non-destructive depth first : based on the previous algorithm but uses the fact that node pointers have a certain alignment to store flags on the least significant bits.  
  The flags tag edges that have been traversed, nodes that haven been already visited and edges that have been overwritten by the algorithm.
* Destructive breadth first : uses the techniques of the 2 algorithms above and needs to overwrite the node content itself to store a counter. It will hold the "node generation",  
  which follows the property __a node generation is the smallest generation of any descendant node__.

## Dependencies

* [graphviz][1] : displays graphs dump in dot files
* perf events : collects hardware counters and profiles
* [flamegraph][0] : outputs a nice representation of profiling data

## Usage

* Go to `sc/main.c` and uncomment what you want to run
* Build the code (2 flavors debug and optimized) : `make dbg|opt`
* Run the code in `bin/dbg/project`
* Generate svg graphs : `make images`
* Profile with perf events (may need root priviledge) : `make dbg|opt_prof`

## Directories

* `bin/` : where build artifacts are stored
* `include/` : header files
* `src/` : the code, tests, and stress tests

[0]: https://github.com/brendangregg/FlameGraph
[1]: http://www.graphviz.org/

