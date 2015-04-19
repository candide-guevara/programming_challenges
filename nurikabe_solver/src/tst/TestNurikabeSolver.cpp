#include <algorithm>
#include<NurikabeSolver.hpp>
#include<unittest.hpp>

namespace EGM {

template <class Solver>
void basicSolvableBoards() {
  Solver solver1 ( R"""(
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R R
    R 0 R R R R R R R
    R R R R 0 R R R R
    R R R R 0 R R R R
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R 81
    )""");
  TEST_ASSERT(solver1.solve(), "Failed solve an all red board -0");

  Solver solver2 ( R"""(
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R R
    R 0 0 0 0 R R R R
    R R 0 0 0 R R R R
    R R 0 0 0 R R R R
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R 76
    )""");
  TEST_ASSERT(solver2.solve(), "Failed to solve and easy board -1");

  Solver solver3 ( R"""(
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R R
    R 0 0 0 0 0 R R R
    R R 0 2 0 R R R R
    R R 0 0 0 R R R R
    R R 0 0 0 R R R R
    R R R R R R R R R
    R R R R R R R R 67
    )""");
  TEST_ASSERT(solver3.solve(), "Failed solve an easy board -2");

  Solver solver4 ( R"""(
    B 4 0 1 B 1 B 1 B
    0 R 0 B B B B B B
    0 0 0 B 1 B 1 B 1
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    )""");
  TEST_ASSERT(solver4.solve(), "Failed solve an easy board -3");

  Solver solver5 ( R"""(
    R R R 0 R R R R R
    R 9 R 0 R R R R 15
    R R R 0 0 R R R R
    0 0 0 0 0 0 0 0 0
    R R 0 0 R R R R R
    R 9 R 0 R R R R 15
    R R R 0 R R R R R
    0 0 0 0 0 0 0 0 0
    R R R 8 R R R 0 0
    )""");
  TEST_ASSERT(solver5.solve(), "Failed solve an less easy board -4");
}

template <class Solver>
void basicUnsolvableBoards() {
  Solver solver0 ( R"""(
    1 0 0 0 0 1 0 0 0
    0 1 0 0 0 0 1 0 0
    0 0 1 0 0 0 0 1 0
    0 0 0 1 0 0 0 0 1
    0 0 0 0 1 0 0 0 0
    1 0 0 0 0 1 0 0 0
    0 1 0 0 0 0 1 0 0
    0 0 1 0 0 0 0 1 0
    0 0 0 1 0 0 0 0 1
    )""");
  TEST_ASSERT(!solver0.solve(), "Failed to solve a trivial unsolvable board -0");

  Solver solver1 ( R"""(
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R R
    R B R R R R R R R
    R R R R 0 R R R R
    R R R R 0 R R R R
    R R R R R R R R R
    R R R R R R R R R
    79 R R R R R R R R
    )""");
  TEST_ASSERT(!solver1.solve(), "Failed to detect easy unsolvable board -1");

  Solver solver2 ( R"""(
    4 0 0 B B 1 B 1 B
    0 0 0 B R B B B B
    0 0 0 B 3 R B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    )""");
  TEST_ASSERT(!solver2.solve(), "Failed to detect easy unsolvable board -2");

  Solver solver3 ( R"""(
    4 R 0 B B B B B B
    R R 0 1 B 1 B 1 B
    0 0 0 B B B B B B
    B 2 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    )""");
  TEST_ASSERT(!solver3.solve(), "Failed to detect easy unsolvable board -3");

  Solver solver4 ( R"""(
    B B B B B B B B B
    B 9 R 0 0 0 0 0 B
    B R R B 0 R 0 0 B
    B R B R R R 0 0 B
    B R 0 0 9 0 0 0 B
    B R 0 R R R B 0 B
    B R 0 0 0 B R R B
    B 0 0 0 0 0 0 4 B
    B B B B B B B B B
    )""");
  TEST_ASSERT(!solver4.solve(), "Failed to detect unsolvable board -4");
}  

template <class Solver>
void complexSolvableBoards() {
  Solver solver1 ( R"""(
    R R R R R R R R 46
    0 0 0 0 0 0 0 0 R
    0 0 0 0 0 R R R R
    0 0 0 0 0 0 0 0 R
    0 0 0 0 R R R R R
    0 0 0 0 0 0 0 0 R
    0 0 0 R R R R R R
    0 0 0 0 0 0 0 0 R
    R R R R R R R R R
    )""");
  TEST_ASSERT(solver1.solve(), "Failed to solve board (many solutions) -1");

  Solver solver2 ( R"""(
    0 R R R R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 64 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R R R 0 0
    0 0 0 0 0 0 0 0 0
    )""");
  TEST_ASSERT(solver2.solve(), "Failed to solve board (fewer solutions) -2");

  Solver solver3 ( R"""(
    B 2 B B B B B B B
    B R B 1 B 0 0 5 B
    B B B B 4 0 0 0 4
    R 0 R R R 0 0 0 R
    R 0 0 0 0 0 0 0 R
    R 0 R 0 R R R 0 R
    4 0 R 0 4 0 0 0 0
    0 5 R R 0 1 0 R 0
    0 0 0 0 0 0 0 2 0
    )""");
  TEST_ASSERT(solver3.solve(), "Failed to solve board (few solutions) -3");

  Solver solver4 ( R"""(
    0 0 0 0 0 0 0 0 0
    R 0 0 0 4 B 4 0 0
    3 0 0 0 B 1 B 0 0
    0 0 0 0 0 B B 0 0
    0 0 2 0 0 B 1 B 0
    0 0 0 0 0 0 B 0 0
    0 R 0 5 0 0 0 0 2
    0 R 3 0 5 0 0 0 0
    0 0 0 0 0 0 0 0 0
    )""");
  TEST_ASSERT(solver4.solve(), "Failed to solve board (few solutions) -4");
  
  Solver solver5 ( R"""(
    0 2 0 0 0 0 0 0 0
    0 0 0 1 0 0 0 5 0
    0 0 0 0 4 0 0 0 4
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    4 0 0 0 4 0 0 0 0
    0 5 0 0 0 1 0 0 0
    0 0 0 0 0 0 0 2 0
    )""");
  TEST_ASSERT(solver5.solve(), "Failed to solve board (few solutions) -4");

  Solver solver6 ( R"""(
    0 0 0 0 0 0 0 0 0
    0 3 0 0 0 0 0 0 0
    4 0 0 0 0 0 0 0 0
    0 0 0 0 2 0 0 5 0
    0 0 0 0 0 0 0 0 0
    0 4 0 0 6 0 0 0 0
    0 0 0 0 0 0 0 0 1
    0 0 0 0 0 0 0 6 0
    0 0 0 0 0 0 0 0 0
    )""");
  TEST_ASSERT(solver6.solve(), "Failed to solve board (few solutions) -5");
}  

template <class Solver>
void complexUnsolvableBoards() {
  Solver solver1 ( R"""(
    4 R 0 3 B B B B B
    R 0 0 0 B R 3 R B
    0 B 0 B B B B B B
    R 0 0 0 0 R R 3 B
    2 0 0 0 0 B B B B
    B R R 3 B R R 3 B
    B B B B B B B B B
    R R R R R R R 8 B
    B B B B B B B B B
    )""");
  TEST_ASSERT(!solver1.solve(), "Failed to detect easy unsolvable board -1");

  Solver solver2 ( R"""(
    R R R 0 B B B B B
    R 9 R 0 0 R R 3 B
    R 0 0 0 0 0 0 0 B
    0 R R 0 R R 0 2 B
    0 R R R 8 0 0 0 B
    0 0 0 0 0 0 0 0 B
    7 R R R R R 0 1 B
    B B B B B B B B B
    0 0 4 0 0 0 0 4 0
    )""");
  TEST_ASSERT(!solver2.solve(), "Failed to detect easy unsolvable board -1");
}  

SIMPLE_TEST(TestSimpleRecursiveSolverBasic) {
  TEST_ASSERT(BaseBoard().xlen() ==  9 && BaseBoard().ylen() == 9, "This test requires a specific board size : 9x9");
  //basicSolvableBoards<SimpleRecursiveSolver<CheckEffort::HIGH, Cell::RED> >();
  //basicUnsolvableBoards<SimpleRecursiveSolver<CheckEffort::HIGH, Cell::RED> >();

  basicSolvableBoards<OptimizedSolver >();
  basicUnsolvableBoards<OptimizedSolver >();
}

SIMPLE_TEST(TestSimpleRecursiveSolverComplex) {
  TEST_ASSERT(BaseBoard().xlen() ==  9 && BaseBoard().ylen() == 9, "This test requires a specific board size : 9x9");
  //complexSolvableBoards<SimpleRecursiveSolver<CheckEffort::HIGH, Cell::RED> >();
  //complexUnsolvableBoards<SimpleRecursiveSolver<CheckEffort::HIGH, Cell::RED> >();

  complexSolvableBoards<OptimizedSolver>();
  complexUnsolvableBoards<OptimizedSolver>();
}

} // namespace EGM

