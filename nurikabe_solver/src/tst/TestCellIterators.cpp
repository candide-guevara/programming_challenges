#include <algorithm>
#include<CellIterators.hpp>
#include<unittest.hpp>

namespace EGM {

SIMPLE_TEST(TestCellItConnected) {
  BaseBoard board;
  board.set(0, 0, Cell::RED);

  ConnectedIt it1(BoardCell(0,0), &board);
  TEST_ASSERT(count_if(++it1, ConnectedIt::END_IT, [](const BoardCell& iCell) {
      LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == 2,
    "Bad number of iterations, expecting 2");

  ConnectedIt it2(BoardCell(1,1), &board);
  TEST_ASSERT(count_if(it2, ConnectedIt::END_IT, [](const BoardCell& iCell) {
      LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == 5,
    "Bad number of iterations, expecting 5");

  ConnectedIt it3(BoardCell(0,1), &board);
  TEST_ASSERT((*++it3).value(board) == Cell::RED , "Bad cell after iterator dereference");
  TEST_ASSERT(it3.toString().size(), "toString method not implemented");
}

SIMPLE_TEST(TestCellItAdjacent) {
  BaseBoard board;
  board.set(1, 0, Cell::BLUE);

  AdjacentIt it1(BoardCell(0,0), &board);
  TEST_ASSERT(count_if(it1, AdjacentIt::END_IT, [](const BoardCell& iCell) {
      LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == 4,
    "Bad number of iterations, expecting 4");

  AdjacentIt it2(BoardCell(1,1), &board);
  TEST_ASSERT(count_if(++it2, AdjacentIt::END_IT, [](const BoardCell& iCell) {
      LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == 8,
    "Bad number of iterations, expecting 8");

  AdjacentIt it3(BoardCell(1,0), &board);
  TEST_ASSERT((*it3).value(board) == Cell::BLUE , "Bad cell after iterator dereference");
}

SIMPLE_TEST(TestCellItColor) {
  BaseBoard board;
  VoidConnectedIt it1(BoardCell(0,0), &board);

  TEST_ASSERT(count_if(it1, VoidConnectedIt::END_IT, [](const BoardCell& iCell) {
      //LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == board.xlen()*board.ylen(),
    "Bad number of iterations, expecting the whole board");

  it1.reset(BoardCell(2,2));

  TEST_ASSERT(count_if(it1, VoidConnectedIt::END_IT, [](const BoardCell& iCell) {
      //LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == board.xlen()*board.ylen(),
    "Bad number of iterations, expecting the whole board -2");

  board.set(1, 1, Cell::BLUE);
  board.set(0, 1, Cell::BLUE);
  board.set(1, 0, Cell::BLUE);
  board.set(0, 0, Cell::BLUE);
  board.set(2, 0, Cell::BLUE);
  board.set(2, 2, Cell::BLUE);
  BlueConnectedIt it2(BoardCell(1,1), &board);

  TEST_ASSERT(count_if(it2, BlueConnectedIt::END_IT, [](const BoardCell& iCell) {
      LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == 5,
    "Bad number of iterations, expecting 5");

  board.set(1, 1, Cell::RED);
  RedConnectedIt it3(BoardCell(1,1), &board);
  TEST_ASSERT(count_if(it3, RedConnectedIt::END_IT, [](const BoardCell& iCell) {
      LOG_DEBUG("Iteration " << iCell.toString());
      return true;
    }) == 1,
    "Bad number of iterations, expecting 1");

  RedConnectedIt it4(BoardCell(1,1), &board);
  TEST_ASSERT(it4.toString().size(), "toString method not implemented");
}  

} // namespace EGM

