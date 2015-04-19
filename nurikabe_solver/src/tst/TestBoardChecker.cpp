#include <algorithm>
#include <BoardChecker.hpp>
#include <unittest.hpp>

namespace EGM {

static const CheckEffort HIGH = CheckEffort::HIGH;
static const CheckEffort LOW = CheckEffort::LOW;

BoardChecker createCheckerFromBoard(const BaseBoard& iBoard) {
  IndexedBoard* idx = new IndexedBoard(iBoard);
  return BoardChecker(idx);
}

BoardChecker createCheckerFromSerializedBoard(const string& iStr) {
  IndexedBoard* idx = new IndexedBoard(iStr);
  return BoardChecker(idx);
}

SIMPLE_TEST(TestBoardCheckerBasic) {
  TEST_ASSERT(BaseBoard().xlen() ==  9 && BaseBoard().ylen() == 9, "This test requires a specific board size : 9x9");

  BoardChecker checker1 = createCheckerFromSerializedBoard( R"""(
    1 0 0 0 0 0 0 0 0
    0 2 0 0 0 B B B B
    B R 0 0 0 B 0 5 0
    0 0 0 0 B 0 B B B
    0 0 0 0 B B 0 0 0
    0 3 0 3 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 1 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    )""");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({8,2}, Cell::RED), "Failed to check number cell -1");
  TEST_ASSERT(checker1.checkNextMoveWithEffort<LOW>({8,2}, Cell::RED), "Failed to check number cell -2");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({5,3}, Cell::BLUE), "Failed to check blue cells -3");
  TEST_ASSERT(checker1.checkNextMoveWithEffort<LOW>({5,3}, Cell::BLUE), "Failed to check blue cells -4");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({2,5}, Cell::RED), "Failed to check red cells -5");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<LOW>({2,5}, Cell::RED), "Failed to check red cells -6");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({1,7}, Cell::RED), "Failed to check red cells -7");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<LOW>({1,7}, Cell::RED), "Failed to check red cells -8");
}

SIMPLE_TEST(TestBoardCheckerSurroundings) {
  TEST_ASSERT(BaseBoard().xlen() ==  9 && BaseBoard().ylen() == 9, "This test requires a specific board size : 9x9");

  BoardChecker checker1 = createCheckerFromSerializedBoard( R"""(
    1 0 0 0 0 0 0 0 0
    0 2 0 0 0 0 0 0 0
    B 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    )""");
  TEST_ASSERT(checker1.checkNextMoveWithEffort<LOW>({1,2}, Cell::RED), "Failed to check number cell -1");
  TEST_ASSERT(checker1.checkNextMoveWithEffort<LOW>({0,1}, Cell::BLUE), "Failed to check number cell -2");

  BoardChecker checker2 = createCheckerFromSerializedBoard( R"""(
    1 0 0 0 0 0 0 0 0
    0 2 0 0 2 2 0 0 0
    0 R 0 0 0 0 0 0 0
    R 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 B R 0 0 0 0 0 0
    0 B B 0 0 0 0 4 0
    0 0 0 0 0 0 0 R B
    0 0 0 0 0 0 0 R B
    )""");
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<LOW>({0,1}, Cell::BLUE), "Failed to check number cell -3");
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<LOW>({2,1}, Cell::RED), "Failed to check number cell -4");
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<HIGH>({2,5}, Cell::BLUE), "Failed to check number cell -5");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({2,5}, Cell::BLUE), "Failed to check number cell -6");
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<HIGH>({8,7}, Cell::RED), "Failed to check number cell -7");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({8,7}, Cell::RED), "Failed to check number cell -8");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<HIGH>({7,7}, Cell::BLUE), "Failed to check number cell -9");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({7,7}, Cell::BLUE), "Failed to check number cell -10");
}  

SIMPLE_TEST(TestBoardCheckerFullBoard) {
  TEST_ASSERT(BaseBoard().xlen() ==  9 && BaseBoard().ylen() == 9, "This test requires a specific board size : 9x9");

  BoardChecker checker1 = createCheckerFromSerializedBoard( R"""(
    R R R B B B B B B
    R 9 R B B B B B B
    R R R B B B B B B
    B B B R R R B B B
    B B B R 9 R B B B
    B B B R R R B B B
    B B B B B B R R B
    B B B B B B R 4 B
    B B B B B B B B B
    )""");
  TEST_ASSERT(!checker1.checkCompletelyFullBoard(), "Failed to check full board -0");

  BoardChecker checker2 = createCheckerFromSerializedBoard( R"""(
    B B B B B B B B B
    B R R R B R R R B
    B R R R B R R R B
    B R R R B R R R B
    B R 21 R B R 21 R B
    B R R R B R R R B
    B R R R B R R R B
    B R R R B R R R B
    B B B B B B B B B
    )""");
  TEST_ASSERT(checker2.checkCompletelyFullBoard(), "Failed to detect a valid full board -1");

  BoardChecker checker3 = createCheckerFromSerializedBoard( R"""(
    B B B B B B B B B
    B R R R B R R R B
    B R R R B R R R B
    B R R R B R R R B
    B R R R B R 21 R B
    B R R R B R R R B
    B R R R B R R R B
    B R R R B R R R B
    B B B B B B B B B
    )""");
  TEST_ASSERT(!checker3.checkCompletelyFullBoard(), "Failed to check full board -2");

  BoardChecker checker4 = createCheckerFromSerializedBoard( R"""(
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    B 1 B 1 B 1 B 1 B
    B B B B B B B B B
    )""");
  TEST_ASSERT(checker4.checkCompletelyFullBoard(), "Failed to check full board -3");
}

SIMPLE_TEST(TestBoardCheckerNextMoveBasic) {
  BoardChecker checker1 = createCheckerFromBoard(BaseBoard());
  TEST_ASSERT(checker1.checkNextMoveWithEffort<HIGH>({0,0}, Cell::BLUE), "Failed to detect valid next move on empty board -1");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({0,0}, Cell::RED), "Failed to check next move on empty board -2");
  TEST_ASSERT(checker1.checkNextMoveWithEffort<LOW>({0,0}, Cell::BLUE), "Failed to detect valid next move on empty board -3");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<LOW>({0,0}, Cell::RED), "Failed to check next move on empty board -4");

  BaseBoard board2;
  fill(board2.begin(), board2.end(), Cell::BLUE);
  BoardChecker checker2 = createCheckerFromBoard(board2);
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<HIGH>({0,0}, Cell::RED), "Failed to check next move on all blue board -5");
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<LOW>({0,0}, Cell::RED), "Failed to check next move on all blue board -6");

  board2.set(0, 0, 2);
  BoardChecker checker3 = createCheckerFromBoard(board2);
  TEST_ASSERT(!checker3.checkNextMoveWithEffort<HIGH>({1,0}, Cell::RED), "Failed to detect valid next move on all blue board -7");
  TEST_ASSERT(checker3.checkNextMoveWithEffort<LOW>({1,0}, Cell::RED), "Failed to detect valid next move on all blue board -8");

  BaseBoard board4;
  fill(board4.begin(), board4.end(), Cell::RED);
  board4.set(0, board4.ylen()-1, board4.xlen()*board4.ylen()-1);
  board4.set(0, 0, Cell::VOID);
  BoardChecker checker4 = createCheckerFromBoard(board4);
  TEST_ASSERT(checker4.checkNextMoveWithEffort<HIGH>({0,0}, Cell::BLUE), "Failed to detect valid move on all red board -9");
  TEST_ASSERT(!checker4.checkNextMoveWithEffort<HIGH>({0,0}, Cell::RED), "Failed to check next move on all red board -10");
  TEST_ASSERT(checker4.checkNextMoveWithEffort<LOW>({0,0}, Cell::BLUE), "Failed to detect valid move on all red board -11");
  TEST_ASSERT(!checker4.checkNextMoveWithEffort<LOW>({0,0}, Cell::RED), "Failed to check next move on all red board -12");
}

SIMPLE_TEST(TestBoardCheckerNextMoveComplex) {
  TEST_ASSERT(BaseBoard().xlen() ==  9 && BaseBoard().ylen() == 9, "This test requires a specific board size : 9x9");

  BoardChecker checker1 = createCheckerFromSerializedBoard( R"""(
    R R R B B B B 1 B
    R 9 R B R 2 B B B
    R R R B B B B 1 B
    0 B B R R R B B B
    B 1 B R 9 R B 1 B
    B B B R R R B B B
    B 1 B B B B R R B
    B B B 4 R B R 4 B
    B 1 B R R B B B B
    )""");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({4,5}, Cell::BLUE), "Failed to check next move on full board +0");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({0,0}, Cell::BLUE), "Failed to check next move on full board +1");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({3,2}, Cell::RED), "Failed to check next move on full board +2");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<HIGH>({8,8}, Cell::RED), "Failed to check next move on full board +3");
  TEST_ASSERT(checker1.checkNextMoveWithEffort<HIGH>({2,2}, Cell::BLUE), "Failed to detect valid next move on full board +4");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<LOW>({4,5}, Cell::BLUE), "Failed to check next move on full board -0");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<LOW>({0,0}, Cell::BLUE), "Failed to check next move on full board -1");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<LOW>({3,2}, Cell::RED), "Failed to check next move on full board -2");
  TEST_ASSERT(!checker1.checkNextMoveWithEffort<LOW>({8,8}, Cell::RED), "Failed to check next move on full board -3");
  TEST_ASSERT(checker1.checkNextMoveWithEffort<LOW>({2,2}, Cell::BLUE), "Failed to detect valid next move on full board -4");

  BoardChecker checker2 = createCheckerFromSerializedBoard( R"""(
    0 0 0 0 0 0 0 B 0
    0 0 0 0 0 0 R 0 0
    0 0 0 0 0 0 R 0 0
    B 0 0 0 0 0 R 0 0
    0 B 0 0 0 0 9 0 B
    B 0 0 0 0 0 R 0 0
    0 0 0 0 0 0 R 0 0
    0 0 0 0 0 0 R 0 0
    0 0 0 0 0 0 R 0 0
    )""");
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<HIGH>({6,0}, Cell::RED), "Failed to check next move on board +5");
  TEST_ASSERT(!checker2.checkNextMoveWithEffort<HIGH>({0,4}, Cell::RED), "Failed to check next move on board +6");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<HIGH>({6,0}, Cell::BLUE), "Failed to detect valid next move on board +7");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<HIGH>({5,1}, Cell::RED), "Failed to detect valid next move on board +8");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<HIGH>({6,8}, Cell::BLUE), "Failed to detect valid next move on board +9");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({6,0}, Cell::RED), "Failed to check next move on board -5");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({0,4}, Cell::RED), "Failed to check next move on board -6");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({6,0}, Cell::BLUE), "Failed to detect valid next move on board -7");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({5,1}, Cell::RED), "Failed to detect valid next move on board -8");
  TEST_ASSERT(checker2.checkNextMoveWithEffort<LOW>({6,8}, Cell::BLUE), "Failed to detect valid next move on board -9");

  BoardChecker checker3 = createCheckerFromSerializedBoard( R"""(
    2 0 0 0 0 B 0 0 0
    0 0 R R R 0 0 2 0
    0 2 0 0 7 0 0 0 0
    0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 3 0 2
    0 0 2 0 0 0 0 3 0
    2 0 0 8 R R R 0 0
    0 0 0 R 0 0 R 0 0
    0 1 0 R 0 0 R 0 3
    )""");
  TEST_ASSERT(!checker3.checkNextMoveWithEffort<HIGH>({7,4}, Cell::RED), "Failed to check next move on board +10");
  TEST_ASSERT(checker3.checkNextMoveWithEffort<HIGH>({4,1}, Cell::BLUE), "Failed to check next move on board +11");
  TEST_ASSERT(!checker3.checkNextMoveWithEffort<HIGH>({4,8}, Cell::BLUE), "Failed to check next move on board +12");
  TEST_ASSERT(checker3.checkNextMoveWithEffort<HIGH>({2,6}, Cell::BLUE), "Failed to detect valid next move on board +13");
  TEST_ASSERT(!checker3.checkNextMoveWithEffort<LOW>({7,4}, Cell::RED), "Failed to check next move on board -10");
  TEST_ASSERT(checker3.checkNextMoveWithEffort<LOW>({4,1}, Cell::BLUE), "Failed to check next move on board -11");
  TEST_ASSERT(!checker3.checkNextMoveWithEffort<LOW>({4,8}, Cell::BLUE), "Failed to check next move on board -12");
  TEST_ASSERT(checker3.checkNextMoveWithEffort<LOW>({2,6}, Cell::BLUE), "Failed to detect valid next move on board -13");

  BoardChecker checker4 = createCheckerFromSerializedBoard( R"""(
    6 0 B B 1 B 1 B 1
    0 0 0 B B B B B B
    0 B B B 1 B 2 B 2
    B B R B B B R B R
    1 B 2 B 1 B B B B
    B B B B B 0 0 0 B
    1 B 1 B 1 B 7 0 B
    B B B B B B B B B
    1 B 1 B 1 B R 2 B
    )""");
  TEST_ASSERT(checker4.checkNextMoveWithEffort<HIGH>({0,1}, Cell::RED), "Failed to detect valid move on board +14");
  TEST_ASSERT(!checker4.checkNextMoveWithEffort<HIGH>({0,1}, Cell::BLUE), "Failed to check next move on board +15");
  TEST_ASSERT(!checker4.checkNextMoveWithEffort<HIGH>({7,6}, Cell::RED), "Failed to check move on board +16");
  TEST_ASSERT(!checker4.checkNextMoveWithEffort<HIGH>({7,6}, Cell::BLUE), "Failed to check next move on board +17");
  TEST_ASSERT(checker4.checkNextMoveWithEffort<LOW>({0,1}, Cell::RED), "Failed to detect valid move on board -14");
  TEST_ASSERT(!checker4.checkNextMoveWithEffort<LOW>({0,1}, Cell::BLUE), "Failed to check next move on board -15");
  TEST_ASSERT(checker4.checkNextMoveWithEffort<LOW>({7,6}, Cell::RED), "Failed to check move on board -16");
  TEST_ASSERT(!checker4.checkNextMoveWithEffort<LOW>({7,6}, Cell::BLUE), "Failed to check next move on board -17");
}

} // namespace EGM


