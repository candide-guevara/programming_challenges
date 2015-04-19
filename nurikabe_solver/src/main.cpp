#include <typeinfo>
#include <NurikabeSolver.hpp>
#include <stl_nurikabe.hpp>

namespace EGM {

void testSimpleBoards() {
  LOG_INFO("------------------- testSimpleBoards -----------------------");

  BaseBoard board;
  readBoardFromString( R"""(
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R R
    R R R R R 0 0 R R
    R R R R R 0 0 R R
    R R R R R R R R R
    R R R R R R 0 R R
    R R R R R R R R R
    R R R R R R R R 81
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);

  readBoardFromString( R"""(
    R R R R R R R R R
    R R R R R R R R R
    R R R R R R R R R
    R R R R R 0 0 R R
    R R R R R 0 0 R R
    R R R R R R R R R
    R R R R R R 0 R R
    R R R R R R R R R
    R R R R R R R R 78
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);

  readBoardFromString( R"""(
    B 2 B B B B B B B
    B R B 1 B 0 0 5 B
    B B B B 4 0 0 0 4
    R 0 R R R 0 0 0 R
    R 0 0 0 0 0 0 0 R
    R 0 R 0 R R R 0 R
    4 0 R 0 4 0 0 0 0
    0 5 R R 0 1 0 R 0
    0 0 0 0 0 0 0 2 0
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);
}

void testWeirdBoards() {
  LOG_INFO("------------------- testWeirdBoards -----------------------");
  BaseBoard board;
  readBoardFromString( R"""(
    R R R R R R R R 46
    0 0 0 0 0 0 0 0 R
    0 0 0 0 0 R R R R
    0 0 0 0 0 0 0 0 R
    0 0 0 0 R R R R R
    0 0 0 0 0 0 0 0 R
    0 0 0 R R R R R R
    0 0 0 0 0 0 0 0 R
    R R R R R R R R R
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);

  readBoardFromString ( R"""(
    0 R R R R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 64 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R 0 0 0 0
    0 0 0 0 R R R 0 0
    0 0 0 0 0 0 0 0 0
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);
}

void testComplexBoards() {
  LOG_INFO("------------------- testComplexBoards -----------------------");
  BaseBoard board;
  readBoardFromString ( R"""(
    0 0 0 0 0 0 0 0 0
    0 0 0 0 4 0 4 0 0
    3 0 0 0 0 1 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 2 0 0 0 1 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 5 0 0 0 0 2
    0 0 3 0 5 0 0 0 0
    0 0 0 0 0 0 0 0 0
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);
  
  readBoardFromString ( R"""(
    0 2 0 0 0 0 0 0 0
    0 0 0 1 0 0 0 5 0
    0 0 0 0 4 0 0 0 4
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    4 0 0 0 4 0 0 0 0
    0 5 0 0 0 1 0 0 0
    0 0 0 0 0 0 0 2 0
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);

  readBoardFromString ( R"""(
    0 0 0 0 0 0 0 0 0
    0 3 0 0 0 0 0 0 0
    4 0 0 0 0 0 0 0 0
    0 0 0 0 2 0 0 5 0
    0 0 0 0 0 0 0 0 0
    0 4 0 0 6 0 0 0 0
    0 0 0 0 0 0 0 0 1
    0 0 0 0 0 0 0 6 0
    0 0 0 0 0 0 0 0 0
    )""", board);
  OptimizedSolver(board).solve();
  stl::solve_board(board);
}

} // namespace EGM

int main (void) {
  LOG_MONIT("Starting ...");

  try {
    // STL solver will not work on this boards but I think they are valid
    // EGM::testSimpleBoards();
    // EGM::testWeirdBoards();
    EGM::testComplexBoards();

  }
  catch (exception& err) {
    LOG_ERROR("catched in main [" << typeid(err).name() << "] : " << err.what());
    return 1;
  }
  catch (...) {
    LOG_ERROR("catched in main: unknown error !");
    return 1;
  }

  LOG_MONIT("All done !!");
  return 0;
}

