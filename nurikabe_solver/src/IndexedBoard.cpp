#include <sstream>
#include <CellIterators.hpp>
#include <IndexedBoard.hpp>

namespace EGM {

IndexedBoard::IndexedBoard(const BaseBoard& iBoard) 
  : _board(iBoard) {
  _numberCells.reserve(iBoard.xlen());
  _blueCells.reserve(iBoard.cellCount());
  _redCells.reserve(iBoard.cellCount());
  _voidCells.reserve(iBoard.cellCount());
  buildIndexes();
}  

IndexedBoard::IndexedBoard(const string& iSerializedBoard) {
  readBoardFromString(iSerializedBoard, _board);
  _blueCells.reserve(_board.cellCount());
  _redCells.reserve(_board.cellCount());
  _voidCells.reserve(_board.cellCount());
  buildIndexes();
}

void IndexedBoard::buildIndexes() {
  auto end = _board.end();
  uint_fast32_t pos = 0;
  uint_fast32_t xlen = _board.xlen();
  _maxRedCount = _maxBlueCount = 0;

  _numberCells.clear();
  _blueCells.clear();
  _redCells.clear();
  _voidCells.clear();

  for(auto it = _board.begin(); it != end; ++it, ++pos) {
    int_type value = *it;
    if ( NumberCellPredicate(value) ) {
      _numberCells.emplace_back(pos%xlen, pos/xlen);
      _maxRedCount += value;
    }  
    if ( RedPredicate(value) )
      _redCells.emplace(pos%xlen, pos/xlen);
    if ( BluePredicate(value) )
      _blueCells.emplace(pos%xlen, pos/xlen);
    if ( VoidPredicate(value) )
      _voidCells.emplace(pos%xlen, pos/xlen);
  }

  _maxBlueCount = _board.cellCount() - _maxRedCount;
  ASSERT(_maxBlueCount >= 0 && _maxRedCount >= 0, "Bad max number of red or blue cells");
}

void IndexedBoard::set(const BoardCell& iCell, int_type value) {
  STAT_START(IndexedBoard::set);
  int_type oldValue = get(iCell);

  if (value == oldValue) return;
  ASSERT(count(_numberCells.cbegin(), _numberCells.cend(), iCell) == 0, "Modifying a number cell"); 

  if ( RedPredicate(value) ) {
    _redCells.insert(iCell);
    _blueCells.erase(iCell);
    _voidCells.erase(iCell);
  }  
  else if ( BluePredicate(value) ) {
    _blueCells.insert(iCell);
    _redCells.erase(iCell);
    _voidCells.erase(iCell);
  }
  else if ( VoidPredicate(value) ) {
    _voidCells.insert(iCell);
    _redCells.erase(iCell);
    _blueCells.erase(iCell);
  }

  _board.set(iCell.x(), iCell.y(), value);
  STAT_STOP(IndexedBoard::set);
}

string IndexedBoard::toString () const {
  stringstream os;
  os << "IndexedBoard : " << _board.toString() << endl
      << "Number index (" << numberSize() << ") : " << toStrHelper(_numberCells) << endl
      << "Red index (" << redSize() << "/" << maxRedCount() << "): " << toStrHelper(_redCells) << endl
      << "Blue index (" << blueSize() << "/" << maxBlueCount() << "): " << toStrHelper(_blueCells) << endl
      << "Void index (" << voidSize() << "): " << toStrHelper(_voidCells);
  return os.str();
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

uint_fast32_t countConnectedByColorHelper (ConnectedIt&& oIt, BoardMask& oiMask, bool (*predicate)(int_type)) {
  uint_fast32_t result = 0;
  //LOG_DEBUG("Iteration " << oIt.toString());
  for (++oIt; oIt != ConnectedIt::END_IT; ++oIt) {
    if ( predicate(oIt.value()) && !oiMask.testAndSet((*oIt).x(), (*oIt).y()) )
      result += 1 + countConnectedByColorHelper(ConnectedIt(*oIt, &oIt.board()), oiMask, predicate);
  }
  return result;
}

uint_fast32_t countConnectedByColor (const BoardCell& iCell, const BaseBoard& iBoard, Predicate_t predicate) {
  STAT_START(countConnectedByColor);
  ASSERT(predicate(iCell.value(iBoard)), "Start cell does not match the predicate");

  BoardMask mask;
  mask.testAndSet(iCell.x(), iCell.y());
  uint_fast32_t result = 1 
    + countConnectedByColorHelper(ConnectedIt(iCell, &iBoard), mask, predicate);

  STAT_STOP(countConnectedByColor);
  return result;
}

uint_fast32_t countConnectedByColor (const BoardCell& iCell, const BaseBoard& iBoard) {
  STAT_START(countConnectedByColor);
  bool (*predicate)(int_type) = NULL;
  if ( BluePredicate(iCell.value(iBoard)) )
    predicate = BluePredicate;
  else if ( RedPredicate(iCell.value(iBoard)) )
    predicate = RedPredicate;
  else if ( VoidPredicate(iCell.value(iBoard)) )
    predicate = VoidPredicate;
  else
    return 0;

  BoardMask mask;
  mask.testAndSet(iCell.x(), iCell.y());
  uint_fast32_t result = 1 
    + countConnectedByColorHelper(ConnectedIt(iCell, &iBoard), mask, predicate);

  STAT_STOP(countConnectedByColor);
  return result;
}

} // namespace EGM


