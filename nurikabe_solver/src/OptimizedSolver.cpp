#include <forward_list>
#include <NurikabeSolver.hpp>

namespace EGM {

ProcessCtl OptimizedSolver::firstSolverIteration () {
  ProcessCtl solved = performExactMovesThenGuess();
  if (solved != ProcessCtl::STOP_OK && solved != ProcessCtl::STOP_KO)
    solved = ProcessCtl::STOP_KO;
  return solved;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

ProcessCtl OptimizedSolver::performExactMovesThenGuess () {
  RandomBoardCellSet voidCells = buildAdjacentVoidCells();
  BoardCellSet rollbackCells ( _idxBoard.cboard().cellCount() );

  ProcessCtl solved = performExactMovesHelper(voidCells, rollbackCells);
  //LOG_DEBUG("After exact moves " << printProgress(solved) << endl << printCurrentCells(voidCells, rollbackCells));

  if (solved == ProcessCtl::CONTINUE) {
    solved = guessThenPerformExactMoves(voidCells, GUESS_COUNT);
  }  

  if (solved != ProcessCtl::STOP_OK && solved != ProcessCtl::STOP_KO) {
    solved = ProcessCtl::CONTINUE;
    rollBackMoves(rollbackCells);
  }

  return solved == ProcessCtl::PRUNE_BRANCH ? ProcessCtl::CONTINUE : solved;
}


ProcessCtl OptimizedSolver::guessThenPerformExactMoves (RandomBoardCellSet& voidCells, uint_fast32_t guessCount) {
  ASSERT(_idxBoard.voidSize() && voidCells.size(), "Should not get an empty board");
  BoardCell cell = *voidCells.begin();
  ASSERT(VoidPredicate( cell.value(_idxBoard.cboard()) ), "Cell must be void " << cell.toString());

  ProcessCtl solved = ProcessCtl::CONTINUE;
  bool successfulMove = false;

  int_type first_choice = _random.nextColor();
  int_type second_choice = Cell::invert(first_choice);

  if ( _checker.checkNextMoveWithEffort<CheckEffort::LOW>(cell, first_choice) ) {
    _idxBoard.set(cell, first_choice);
    successfulMove = true;
    solved = guessOneMoveHelper(voidCells, guessCount);
  }  
  if ( solved == ProcessCtl::CONTINUE && _checker.checkNextMoveWithEffort<CheckEffort::LOW>(cell, second_choice) ) {
    _idxBoard.set(cell, second_choice);
    successfulMove = true;
    solved = guessOneMoveHelper(voidCells, guessCount);
  }  

  if (solved != ProcessCtl::STOP_OK && solved != ProcessCtl::STOP_KO) {
    //LOG_DEBUG("Cannot guess any further for " << cell.toString() << " " << printProgress(solved) << endl << printCurrentCells(voidCells));
  }
  else {
    solved = successfulMove || guessCount < GUESS_COUNT ? solved : ProcessCtl::STOP_KO;
    ASSERT(solved != ProcessCtl::STOP_KO, "Did not do a thing for these : " << toStrHelper(voidCells));
  }
  return solved;
}

ProcessCtl OptimizedSolver::guessOneMoveHelper (RandomBoardCellSet& voidCells, uint_fast32_t guessCount) {
  const BoardCell cell = *voidCells.begin();
  ProcessCtl solved = ProcessCtl::CONTINUE;
  ASSERT(guessCount, "Bad guess count");
  --guessCount;
  ++_progress;

  if ( !_idxBoard.voidSize() ) {
    solved = _checker.checkCompletelyFullBoard() ? ProcessCtl::STOP_OK : ProcessCtl::CONTINUE;
  }  
  else if (voidCells.size() > 1 && guessCount) {
    //LOG_DEBUG("Recurse guess again " << cell.toString() << " " << printProgress(solved) << endl << printCurrentCells(voidCells));

    voidCells.erase(voidCells.begin());
    solved = guessThenPerformExactMoves (voidCells, guessCount);
    voidCells.insert( voidCells.begin(), cell );
  }  
  else {
    //LOG_DEBUG("Recurse perform exact moves " << printProgress(solved) << endl << printCurrentCells(voidCells));
    solved = performExactMovesThenGuess ();
  }

  ASSERT(solved != ProcessCtl::PRUNE_BRANCH, "Cannot prune at this point");
  if (solved != ProcessCtl::STOP_OK && solved != ProcessCtl::STOP_KO)
    _idxBoard.set(cell, Cell::VOID);
  return solved;
}

ProcessCtl OptimizedSolver::performExactMovesHelper (RandomBoardCellSet& voidCells, BoardCellSet& rollbackCells) {
  ProcessCtl solved = ProcessCtl::CONTINUE;
  uint_fast32_t resolvedCount = 0;

  for (auto it=voidCells.begin(), end=voidCells.end(); it != end;) {
    int_type color = Cell::BLUE;
    const BoardCell& cell = *it;
    ASSERT(VoidPredicate( cell.value(_idxBoard.cboard()) ), "Cell must be void " << cell.toString());

    uint_fast32_t result = _checker.checkNextMoveWithEffort<CheckEffort::HIGH>(cell, Cell::RED)
      + _checker.checkNextMoveWithEffort<CheckEffort::HIGH>(cell, Cell::BLUE) * 2;

    switch (result) {
      case 0: 
        //LOG_DEBUG("Bad branch detected during exact moves " << cell.toString() << " "
        //  << printProgress(solved) << endl << printCurrentCells(voidCells, rollbackCells));
        return ProcessCtl::PRUNE_BRANCH;
      case 1: 
        color = Cell::RED;
      case 2: 
        ++resolvedCount;
        rollbackCells.insert(cell);
        _idxBoard.set(cell, color);
        voidCells.erase(it++);
        break;
      default: ++it;  
    }
  }

  _progress += resolvedCount;
  if ( !_idxBoard.voidSize() )
    solved = _checker.checkCompletelyFullBoard() ? ProcessCtl::STOP_OK : ProcessCtl::PRUNE_BRANCH;
  else if ( resolvedCount ) {
    buildQuicklyAdjacentVoidCells(voidCells, rollbackCells);
    ASSERT(voidCells.size(), "There should always be void adjacent void cells on non empty board");
    solved = performExactMovesHelper(voidCells, rollbackCells);
  }  
  return solved;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

RandomBoardCellSet OptimizedSolver::buildAdjacentVoidCells () const {
  ConnectedIt it(BoardCell(0,0), &_idxBoard.cboard());
  RandomBoardCellSet adjCells ( _idxBoard.cboard().cellCount() );

  for (auto redIt = _idxBoard.redBegin(), end = _idxBoard.redEnd(); redIt != end; ++redIt) {
    it.reset(*redIt);
    for (++it; it != ConnectedIt::END_IT; ++it)
      if ( VoidPredicate(it.value()) )
        adjCells.insert(*it);
  }
  for (auto blueIt = _idxBoard.blueBegin(), end = _idxBoard.blueEnd(); blueIt != end; ++blueIt) {
    it.reset(*blueIt);
    for (++it; it != ConnectedIt::END_IT; ++it)
      if ( VoidPredicate(it.value()) )
        adjCells.insert(*it);
  }
  return adjCells;
}

void OptimizedSolver::buildQuicklyAdjacentVoidCells (RandomBoardCellSet& voidCells, const BoardCellSet& previousRoundCells) const {
  ConnectedIt it(BoardCell(0,0), &_idxBoard.cboard());

  for (auto prevIt = previousRoundCells.begin(), end = previousRoundCells.end(); prevIt != end; ++prevIt) {
    if ( !VoidPredicate( prevIt->value(_idxBoard.cboard()) ) ) {
      it.reset(*prevIt);

      for (++it; it != ConnectedIt::END_IT; ++it)
        if ( VoidPredicate(it.value()) )
          voidCells.insert(*it);
    }    
  }
}

void OptimizedSolver::rollBackMoves (const BoardCellSet& modifCells) {
  if ( modifCells.size() > 10 ) {
    for (const auto& cell : modifCells)
      _idxBoard.board().set(cell.x(), cell.y(), Cell::VOID);
    _idxBoard.buildIndexes();  
  }
  else
    for (const auto& cell : modifCells)
      _idxBoard.set(cell.x(), cell.y(), Cell::VOID);
  //LOG_DEBUG("Rolled back moves " << toStrHelper(modifCells) << " :" << printProgress(solved));
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

string OptimizedSolver::printCurrentCells (const RandomBoardCellSet& voidCells, const BoardCellSet& rollbackCells) const {
  stringstream os;
  os << "Void : " << toStrHelper(voidCells) << " / Rollback" << toStrHelper(rollbackCells);
  return os.str();
}

} // namespace EGM

