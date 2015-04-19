#include <fstream>
#include<BaseBoard.hpp>
#include<unittest.hpp>

namespace EGM {

SIMPLE_TEST(TestBaseBoardSet) {
  BaseBoard board;
  TEST_ASSERT(board.get(0,0) == Cell::VOID, "Cells should be initialized to the void value -1");
  TEST_ASSERT(board.get(board.xlen()-1,0) == Cell::VOID, "Cells should be initialized to the void value -2");
  TEST_ASSERT(board.get(0,board.ylen()-1) == Cell::VOID, "Cells should be initialized to the void value -3");

  board.set(0, 0, Cell::RED);
  TEST_ASSERT(board.get(0,0) == Cell::RED, "Failed to set value -1");
  board.set(0, 0, Cell::BLUE);
  TEST_ASSERT(board.get(0,0) == Cell::BLUE, "Failed to set value -2");
}

SIMPLE_TEST(TestBaseBoardPrint) {
  BaseBoard board;
  board.set(0, 0, Cell::RED);
  board.set(board.xlen()-1, 0, Cell::BLUE);
  board.set(0, board.ylen()-1, 5);

  string str = board.toString();
  TEST_ASSERT(str.size() >= board.xlen()*board.ylen(), "Board to string failed, string seems too short");
  LOG_DEBUG("Result board : " << str);
}  

SIMPLE_TEST(TestBaseBoardSerialize) {
  BaseBoard board, loadedBoard;
  board.set(0, 0, 5);
  board.set(board.xlen()-1, 0, Cell::RED);
  board.set(0, board.ylen()-1, Cell::BLUE);

  readBoardFromString(board.serialize(), loadedBoard);

  TEST_ASSERT(loadedBoard.get(0,0) == 5, "Read bad value from serialized board -1");
  TEST_ASSERT(loadedBoard.get(loadedBoard.xlen()-1,0) == Cell::RED, "Read bad value from serialized board -2");
  TEST_ASSERT(loadedBoard.get(0,loadedBoard.ylen()-1) == Cell::BLUE, "Read bad value from serialized board -3");
}  

SIMPLE_TEST(TestBaseBoardRead) {
  BaseBoard fileBoard, strBoard;
  string fileName = "board_dump.txt";
  stringstream os;

  for (uint_fast32_t x=0, y=0; y < fileBoard.ylen(); 
        x = (x+1) % fileBoard.xlen() , y += (!x ? 1 : 0) ) {
    if (!x) os << endl;    
    if (!x && y == fileBoard.ylen()-1) 
      os << Cell::BLUE;
    else if (!y && x == fileBoard.xlen()-1) 
      os << Cell::RED;
    else
      os << Cell::VOID;
    os << ", ";
  }
  LOG_DEBUG("File content : " << endl << os.str());
  {
  ofstream of (fileName);
  of << os.str();
  }
  readBoardFromString(os.str(), strBoard);
  readBoardFromFile(fileName, fileBoard);

  TEST_ASSERT(strBoard.get(0,0) == Cell::VOID, "Read bad value from string -1");
  TEST_ASSERT(strBoard.get(strBoard.xlen()-1,0) == Cell::RED, "Read bad value from string -2");
  TEST_ASSERT(strBoard.get(0,strBoard.ylen()-1) == Cell::BLUE, "Read bad value from string -3");

  TEST_ASSERT(fileBoard.get(0,0) == Cell::VOID, "Read bad value from file -1");
  TEST_ASSERT(fileBoard.get(fileBoard.xlen()-1,0) == Cell::RED, "Read bad value from file -2");
  TEST_ASSERT(fileBoard.get(0,fileBoard.ylen()-1) == Cell::BLUE, "Read bad value from file -3");
}  

} // namespace EGM

