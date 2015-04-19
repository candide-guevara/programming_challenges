#include <algorithm>
#include<IndexedBoard.hpp>
#include<RandomColorSeq.hpp>
#include<unittest.hpp>

namespace EGM {

SIMPLE_TEST(TestIndexedBoardCountByColor) {
  BaseBoard board;

  TEST_ASSERT(countConnectedByColor(BoardCell(1,1),board) == board.xlen()*board.ylen(),
    "Bad number of iterations, expecting the whole board");
  TEST_ASSERT(countConnectedByColor(BoardCell(board.xlen()-1,board.ylen()-1),board) == board.xlen()*board.ylen(),
    "Bad number of iterations, expecting the whole board -2");

  board.set(0, 0, 2);
  board.set(1, 1, Cell::RED);
  board.set(1, 0, Cell::RED);
  board.set(0, 1, Cell::RED);
  board.set(0, 2, Cell::RED);

  TEST_ASSERT(countConnectedByColor(BoardCell(0,0),board) == 5,
    "Bad number of iterations, expecting 5");

  board.set(0, 0, Cell::BLUE);
  TEST_ASSERT(countConnectedByColor(BoardCell(0,0),board) == 1,
    "Bad number of iterations, expecting only 1");
}

SIMPLE_TEST(TestRandomBoardCellSet) {
  const uint_fast32_t rowLen = 9, cellCount=rowLen*rowLen;
  uint_fast32_t pos = 0;
  RandomBoardCellSet s1(cellCount), s2(cellCount);
  array<BoardCell, cellCount> cells;

  for (auto it=cells.begin(), end=cells.end(); it != end; ++it, ++pos)
    *it = BoardCell(pos % rowLen, pos / rowLen);
  
  s1.insert(cells.begin(), cells.end());
  s2.insert(cells.begin(), cells.end());

  LOG_DEBUG("Random cell sequence 1 " << toStrHelper(s1));
  LOG_DEBUG("Random cell sequence 2 " << toStrHelper(s2));
  ASSERT(equal (s1.begin(), s1.end(), s2.begin()) == false, "Sequences must be different");
}

} // namespace EGM

