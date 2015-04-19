#include <sstream>
#include <set>
#include <BoardChecker.hpp>

namespace EGM {

BoardChecker::BoardChecker(IndexedBoard* idxBoard) 
  : _idxBoard(idxBoard) 
{ 
  ASSERT(_idxBoard, "NULL"); 
}

bool BoardChecker::checkCompletelyFullBoard() const {
  STAT_START(BoardChecker::checkCompletelyFullBoard);
  ASSERT(_idxBoard->voidSize() == 0, "Called on a non-full board");
  bool result = false;

  result = !_idxBoard->blueSize() 
    || checkDeeplyEmptyOrFullBlueBoard<BlueConnectedIt,0>( *_idxBoard->blueBegin() );
  result &= checkDeeplyFullRedBoard();

  //LOG_DEBUG("Correctness result=" << result << ", for " << _idxBoard->toString());
  STAT_STOP(BoardChecker::checkCompletelyFullBoard);
  return result;
}

template <CheckEffort Effort>
bool BoardChecker::checkNextMoveWithEffort(const BoardCell& iCell, int_type iNextValue) {
  STAT_START(BoardChecker::checkNextMoveWithEffort);
  const int_type previousValue = iCell.value(_idxBoard->cboard());
  _idxBoard->board().set(iCell.x(), iCell.y(), iNextValue);

  ASSERT(!NumberCellPredicate(previousValue) 
    && !VoidPredicate(iNextValue) && !NumberCellPredicate(iNextValue) && iNextValue != previousValue, 
    "Next move cannot be " << iCell.toString() << "->" << Cell::toString(iNextValue));

  uint_fast32_t moveType = VoidPredicate(previousValue) + 
    2 * BluePredicate(previousValue) + 4 * BluePredicate(iNextValue);

  bool result = checkNextMoveWithEffortHelper<Effort>(iCell, static_cast<MoveType>(moveType));  

  _idxBoard->board().set(iCell.x(), iCell.y(), previousValue);
  //LOG_DEBUG("With next move : " << iCell.toString() << "->" << Cell::toString(iNextValue) << endl
  //  << "Correctness result=" << result << ", for " << _idxBoard->toString());
  STAT_STOP(BoardChecker::checkNextMoveWithEffort);
  return result;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <>
bool BoardChecker::checkNextMoveWithEffortHelper<CheckEffort::HIGH> (const BoardCell& iCell, MoveType iType) {
  bool result = true;
  switch (iType) {

  case MoveType::VOID_TO_RED:
    result = _idxBoard->redSize() < _idxBoard->maxRedCount()
      && checkDeeplyNextMoveRed(iCell) 
      && checkQuicklySurroundingBlueCells<AdjacentIt,0>(iCell);
    break;  
    
  case MoveType::BLUE_TO_RED:
    result = _idxBoard->redSize() < _idxBoard->maxRedCount()
      && checkDeeplyNextMoveRed(iCell) 
      && checkQuicklySurroundingBlueCells<AdjacentIt,1>(iCell);
    break;  

  case MoveType::RED_TO_BLUE:
  case MoveType::VOID_TO_BLUE:
    result = _idxBoard->blueSize() < _idxBoard->maxBlueCount()
      && checkDeeplyEmptyOrFullBlueBoard<BlueOrVoidConnectedIt,-1>(iCell)
      && checkQuicklySurroundingRedCells<ConnectedIt>(iCell);
    break;

  default:
    ASSERT(false, "Next move cannot be " << iCell.toString() << "->" << Cell::toString(iCell.value(_idxBoard->cboard())) );
  }
  return result;
}

template <>
bool BoardChecker::checkNextMoveWithEffortHelper<CheckEffort::LOW> (const BoardCell& iCell, MoveType iType) {
  bool result = true;
  switch (iType) {

  case MoveType::VOID_TO_RED:
  case MoveType::BLUE_TO_RED:
    result = _idxBoard->redSize() < _idxBoard->maxRedCount()
      && checkQuicklyNextMoveRed(iCell);
    break;  

  case MoveType::RED_TO_BLUE:
  case MoveType::VOID_TO_BLUE:
    result = _idxBoard->blueSize() < _idxBoard->maxBlueCount()
      && checkQuicklyEmptyOrFullBlueBoard<BlueOrVoidConnectedIt,-1>(iCell)
      && checkQuicklySurroundingRedCells<ConnectedIt>(iCell);
    break;

  default:
    ASSERT(false, "Next move cannot be " << iCell.toString() << "->" << Cell::toString(iCell.value(_idxBoard->cboard())) );
  }
  return result;
}

template bool BoardChecker::checkNextMoveWithEffort<CheckEffort::HIGH>(const BoardCell&, int_type);
template bool BoardChecker::checkNextMoveWithEffort<CheckEffort::LOW>(const BoardCell&, int_type);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <class Iterator, int_fast32_t CountOffset>
bool BoardChecker::checkQuicklySurroundingBlueCells(const BoardCell& iStartCell) const {
  bool result = true;
  for (Iterator it(iStartCell, &_idxBoard->cboard()); ++it != Iterator::END_IT; ) {
    if ( BluePredicate(it.value()) ) {
      result = checkQuicklyEmptyOrFullBlueBoard<BlueOrVoidConnectedIt,CountOffset>(*it);
      break;
    }  
  }
  return result;
}

template <class Iterator>
bool BoardChecker::checkQuicklySurroundingRedCells(const BoardCell& iStartCell) const {
  bool result = true;
  for (Iterator it(iStartCell, &_idxBoard->cboard()); result && ++it != Iterator::END_IT; ) {
    int_type value = it.value();
    if ( NumberCellPredicate(value) )
      result = checkQuicklyNumberCell(*it);
    else if ( RedPredicate(value) )
      result = checkQuicklyNextMoveRed(*it);
  }
  return result;
}

bool BoardChecker::checkDeeplyNextMoveRed(const BoardCell& iStartCell) const {
  ASSERT(RedPredicate( iStartCell.value(_idxBoard->cboard()) ), "Not a red cell");
  bool hasEnoughFreeSpace = true;
  bool canReachNumberCell = true;
  bool tooManyNumberCell = false;
  uint_fast32_t redCount = 1;
  uint_fast32_t maxRedCount = 0;

  for (RedConnectedIt redIt(iStartCell, &_idxBoard->cboard()); 
        ++redIt != RedConnectedIt::END_IT && !tooManyNumberCell && hasEnoughFreeSpace; ++redCount) {
    int_type tmp = redIt.value();
    if ( NumberCellPredicate(tmp) ) {
      tooManyNumberCell = maxRedCount ? true : false;
      maxRedCount = tmp;
      hasEnoughFreeSpace = checkQuicklyNumberCell(*redIt);
    }  
  }  

  if (!maxRedCount) {
    //LOG_DEBUG("No number cell connected to " << iStartCell.toString());
    canReachNumberCell = checkQuicklyCanReachNumberCell(iStartCell);
  }  
  else if (tooManyNumberCell) {
    //LOG_DEBUG("More than one number cell connected to " << iStartCell.toString());
  }  
  else if (redCount > maxRedCount) {
    //LOG_DEBUG("Expecting " << maxRedCount << " red cells, got " << redCount << " at " << iStartCell.toString());
  }  
  return canReachNumberCell && hasEnoughFreeSpace && !tooManyNumberCell && (!maxRedCount || redCount <= maxRedCount);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool BoardChecker::checkDeeplyFullRedBoard() const {
  if (!_idxBoard->numberSize()) return true;

  bool result = true;
  auto it = _idxBoard->numberBegin(), end = _idxBoard->numberEnd();

  for (RedConnectedIt redIt(*it, &_idxBoard->cboard()); result && it != end; ++it) {
    uint_fast32_t redCount = 1;
    redIt.reset(*it);

    for (; result && ++redIt != RedConnectedIt::END_IT; ++redCount) {
      result &= !NumberCellPredicate( redIt.value() );
      if (!result) {
        //LOG_DEBUG("Two number cells are connected " << it->toString() << " and " << (*redIt).toString() );
      }  
    }

    result &= it->value(_idxBoard->cboard()) == redCount;
    if (!result) {
      //LOG_DEBUG("Failed check on connected red cells starting at " << it->toString());
    }  
  }    

  if (result) {
    result &= _idxBoard->redSize() == _idxBoard->maxRedCount();
    if (!result) {
      //LOG_DEBUG("Failed check, some red cells are not connected to any number"); 
    }  
  }  
  return result;
}

template <class Iterator, int_fast32_t CountOffset>
bool BoardChecker::checkDeeplyEmptyOrFullBlueBoard(const BoardCell& iStartCell) const {
  ASSERT(BluePredicate( iStartCell.value(_idxBoard->cboard()) ), "Not a blue cell");

  bool noPoolFound = true;
  int_fast32_t remainToVisit = _idxBoard->blueSize() - CountOffset;
  PoolIt poolIt(iStartCell, &_idxBoard->cboard());

  for (Iterator blueIt(iStartCell, &_idxBoard->cboard()); 
        noPoolFound && remainToVisit > 0 && blueIt != Iterator::END_IT; ++blueIt) {
    uint_fast32_t consecutiveBlue = 0;

    if ( BluePredicate( blueIt.value() ) ) {
      --remainToVisit;

      for (poolIt.reset(*blueIt); ++poolIt != PoolIt::END_IT && BluePredicate( poolIt.value() ); 
           ++consecutiveBlue);

      noPoolFound = consecutiveBlue < 3;
      if (!noPoolFound) {
        //LOG_DEBUG("Found a pool of blue cells at " << (*blueIt).toString());
      }  
    }
  }

  if (noPoolFound && remainToVisit > 0) {
    //LOG_DEBUG("Failed check on connected blue cells starting at " << iStartCell.toString());
  }  
  return noPoolFound && remainToVisit <= 0;
}

template <class Iterator, int_fast32_t CountOffset>
bool BoardChecker::checkQuicklyEmptyOrFullBlueBoard(const BoardCell& iStartCell) const {
  ASSERT(BluePredicate( iStartCell.value(_idxBoard->cboard()) ), "Not a blue cell");

  bool noPoolFound = true;
  uint_fast32_t consecutiveBlue = 0;
  int_fast32_t remainToVisit = _idxBoard->blueSize() - CountOffset - 1;
  
  for (PoolIt poolIt(iStartCell, &_idxBoard->cboard()); 
        ++poolIt != PoolIt::END_IT && BluePredicate( poolIt.value() ); ++consecutiveBlue);
  noPoolFound = consecutiveBlue < 3;

  if (noPoolFound) {
    for (Iterator blueIt(iStartCell, &_idxBoard->cboard()); 
          remainToVisit > 0 && ++blueIt != Iterator::END_IT; 
          remainToVisit -= BluePredicate( blueIt.value() ) ? 1 : 0
    );      

    if (remainToVisit > 0) {
      //LOG_DEBUG("Failed check on connected blue cells starting at " << iStartCell.toString());
    }  
  }  
  else {
    //LOG_DEBUG("Found a pool of blue cells at " << iStartCell.toString());
  }
  return noPoolFound && remainToVisit <= 0;
}

bool BoardChecker::checkQuicklyNextMoveRed(const BoardCell& iStartCell) const {
  ASSERT(RedPredicate( iStartCell.value(_idxBoard->cboard()) ), "Not a red cell");
  uint_fast32_t redCount = 1;
  uint_fast32_t maxRedCount = 0;

  for (RedConnectedIt redIt(iStartCell, &_idxBoard->cboard()); 
        ++redIt != RedConnectedIt::END_IT && maxRedCount != Cell::INVALID_MAX_NUMBER; ++redCount) {
    int_type tmp = redIt.value();
    if ( NumberCellPredicate(tmp) )
      maxRedCount = !maxRedCount ? tmp : Cell::INVALID_MAX_NUMBER;
  }  

  if (!maxRedCount) {
    //LOG_DEBUG("No number cell connected to " << iStartCell.toString());
  }  
  else if (maxRedCount == Cell::INVALID_MAX_NUMBER) {
    //LOG_DEBUG("More than one number cell connected to " << iStartCell.toString());
  }  
  else if (redCount > maxRedCount) {
    //LOG_DEBUG("Expecting " << maxRedCount << " red cells, got " << redCount << " at " << iStartCell.toString());
  }  
  return !maxRedCount || (redCount <= maxRedCount && maxRedCount != Cell::INVALID_MAX_NUMBER);
}

bool BoardChecker::checkQuicklyNumberCell (const BoardCell& iStartCell) const {
  int_type value = iStartCell.value( _idxBoard->cboard() );
  uint_fast32_t redOrVoidCount = 0;
  ASSERT(NumberCellPredicate( value ), "Not a number cell");

  for (RedOrVoidConnectedIt redIt(iStartCell, &_idxBoard->cboard()); 
    (++redOrVoidCount < value) && (++redIt != RedOrVoidConnectedIt::END_IT); );
  
  bool hasEnoughFreeSpace = redOrVoidCount == value;
  if (!hasEnoughFreeSpace) {
    //LOG_DEBUG("Not enough free space for " << Cell::toString(value) << " cells at " << iStartCell.toString());
  }  
  return hasEnoughFreeSpace;
}

bool BoardChecker::checkQuicklyCanReachNumberCell (const BoardCell& iStartCell) const {
  bool canReachNumberCell = false;
  ASSERT(RedPredicate( iStartCell.value(_idxBoard->cboard()) ), "Not a red cell");

  for (RedOrVoidConnectedIt redIt(iStartCell, &_idxBoard->cboard()); 
      !canReachNumberCell && ++redIt != RedOrVoidConnectedIt::END_IT; )
    canReachNumberCell = NumberCellPredicate( redIt.value() );
  
  if (!canReachNumberCell) {
    //LOG_DEBUG("Cannot reach a number cell from " << iStartCell.toString());
  }  
  return canReachNumberCell;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

string BoardChecker::toString() const {
  stringstream os;
  os << "BoardChecker=" << _idxBoard;
  return os.str();
}

} //namespace EGM

