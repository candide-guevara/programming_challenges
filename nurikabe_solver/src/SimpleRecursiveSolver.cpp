#include <sstream>
#include <NurikabeSolver.hpp>

namespace EGM {

BaseSolver::BaseSolver(const BaseBoard& iBoard) 
  : _idxBoard(iBoard), _checker(&_idxBoard) {}

BaseSolver::BaseSolver(const string& iSerialized)
  : _idxBoard(iSerialized), _checker(&_idxBoard) {}

bool BaseSolver::solve() {
  ASSERT(_idxBoard.voidSize(), "No need to solve a full board");
  ASSERT(_idxBoard.numberSize(), "Invalid board : no number cells");
  LOG_INFO("Attempting to solve : " << _idxBoard.toString());

  STAT_HIGH_PRIO_START(NurikabeSolver::solve);

  _progress = 0;
  ProcessCtl result = firstSolverIteration();
  bool solved = ctlToBool( result );

  STAT_HIGH_PRIO_STOP(NurikabeSolver::solve);

  ASSERT(result == ProcessCtl::STOP_KO || result == ProcessCtl::STOP_OK,
    "Was not expecting this state at the end of the process : " << EGM::toString(result));

  if (!solved)
    LOG_WARN("No solution found for (progress=" << _progress << ") : " << _idxBoard.cboard().toString());
  else
    LOG_INFO("Found a solution (progress=" << _progress << ") : " << _idxBoard.cboard().toString());
  return solved;
}

string toString (ProcessCtl ctl) {
  switch (ctl) {
    case ProcessCtl::STOP_KO:
      return "STOP_KO";
    case ProcessCtl::STOP_OK:
      return "STOP_OK";
    case ProcessCtl::PRUNE_BRANCH:
      return "PRUNE_BRANCH";
    case ProcessCtl::CONTINUE:
      return "CONTINUE";
  }
}

string BaseSolver::printProgress(ProcessCtl control) const {
  stringstream os;
  os << "(progress=" << _progress << ", ctl=" << EGM::toString(control) 
    << ") :" << _idxBoard.cboard().toString();
  return os.str();
}

string BaseSolver::toString() const {
  stringstream os;
  os << "Solver : " << typeid(*this).name() << " at " << this << endl
    << "Engine : " << _random.toString();
  return os.str();
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<CheckEffort Level>
ProcessCtl SimpleRecursiveSolver<Level>::firstSolverIteration() {
  ProcessCtl solved = ProcessCtl::CONTINUE;
  BoardCell cell = *_idxBoard.voidBegin();

  if ( _checker.checkNextMoveWithEffort<Level>(cell, Cell::RED) ) {
    _idxBoard.set(cell, Cell::RED);
    solved = recursiveSolverHelper();
  }

  if ( solved == ProcessCtl::CONTINUE && _checker.checkNextMoveWithEffort<Level>(cell, Cell::BLUE) ) {
    _idxBoard.set(cell, Cell::BLUE);
    solved = recursiveSolverHelper();
  }
  return solved == ProcessCtl::CONTINUE ? ProcessCtl::STOP_KO : solved;
}

template<CheckEffort Level>
ProcessCtl SimpleRecursiveSolver<Level>::recursiveSolverHelper() {
  ProcessCtl solved = ProcessCtl::CONTINUE;

  if ( !_idxBoard.voidSize() ) {
    solved = _checker.checkCompletelyFullBoard() ? ProcessCtl::STOP_OK : ProcessCtl::PRUNE_BRANCH;
    if (solved != ProcessCtl::STOP_OK ) {
      //LOG_DEBUG("Full board is not correct at (progress=" << _progress << ") : " << _idxBoard.cboard().toString());
    }  
  }  
  else if ( !(++_progress % INTERVAL_BETWEEN_HOOK) )
    solved = processEveryNStepsHook();

  if (solved == ProcessCtl::CONTINUE) {
    const BoardCell cell = *_idxBoard.voidBegin();
    int_type first_choice = _random.nextColor();
    int_type second_choice = Cell::invert(first_choice);

    if ( _checker.checkNextMoveWithEffort<Level>(cell, first_choice) ) {
      _idxBoard.set(cell, first_choice);
      solved = recursiveSolverHelper();
    }
    if ( solved == ProcessCtl::CONTINUE && _checker.checkNextMoveWithEffort<Level>(cell, second_choice) ) {
      _idxBoard.set(cell, second_choice);
      solved = recursiveSolverHelper();
    }
    if (solved != ProcessCtl::STOP_OK && solved != ProcessCtl::STOP_KO) {
      //LOG_DEBUG("Giving up on this branch at (progress=" << _progress << ") : " << cell.toString() << _idxBoard.cboard().toString());
      _idxBoard.set(cell, Cell::VOID);
    }
  }
  return solved == ProcessCtl::PRUNE_BRANCH ? ProcessCtl::CONTINUE : solved;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<CheckEffort Level>
ProcessCtl SimpleRecursiveSolver<Level>::processEveryNStepsHook() {
  //LOG_DEBUG("Solver progress (progress=" << _progress << ") :" << _idxBoard.toString());
  return ProcessCtl::CONTINUE;
}

template class SimpleRecursiveSolver<CheckEffort::HIGH>;
template class SimpleRecursiveSolver<CheckEffort::LOW>;

} // namespace EGM

