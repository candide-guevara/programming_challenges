#ifndef NURIKABE_SOLVER_H_
#define NURIKABE_SOLVER_H_

#include <common.hpp>
#include <BoardChecker.hpp>
#include <RandomColorSeq.hpp>

namespace EGM {

enum class ProcessCtl { STOP_KO, STOP_OK, PRUNE_BRANCH, CONTINUE };
inline bool ctlToBool (ProcessCtl ctl) { return ctl == ProcessCtl::STOP_OK; }
string toString (ProcessCtl ctl);

class BaseSolver {
  public:
    BaseSolver(const BaseBoard& iBoard);
    BaseSolver(const string& iSerialized);
    
    bool solve();
    virtual string toString() const;

  protected:
    virtual ProcessCtl firstSolverIteration() = 0;
    virtual string printProgress(ProcessCtl control) const;

    uint64_t _progress;
    IndexedBoard _idxBoard;
    BoardChecker _checker;
    MyRandomColorSeq _random;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<CheckEffort Level>
class SimpleRecursiveSolver : public BaseSolver {
  public:
    using BaseSolver::BaseSolver;

  protected:
    ProcessCtl recursiveSolverHelper();
    virtual ProcessCtl firstSolverIteration();
    ProcessCtl processEveryNStepsHook();

    const static uint64_t INTERVAL_BETWEEN_HOOK = 1024;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class OptimizedSolver : public BaseSolver {
  public:
    using BaseSolver::BaseSolver;
    
  private:
    virtual ProcessCtl firstSolverIteration();

    ProcessCtl performExactMovesThenGuess ();
    ProcessCtl performExactMovesHelper (RandomBoardCellSet& voidCells, BoardCellSet& rollbackCells);
    ProcessCtl guessThenPerformExactMoves (RandomBoardCellSet& voidCells, uint_fast32_t guessCount);
    ProcessCtl guessOneMoveHelper (RandomBoardCellSet& voidCells, uint_fast32_t guessCount);

    RandomBoardCellSet buildAdjacentVoidCells () const;
    void buildQuicklyAdjacentVoidCells (RandomBoardCellSet& voidCells, const BoardCellSet& previousRoundCells) const;
    void rollBackMoves (const BoardCellSet& modifCells);

    string printCurrentCells (const RandomBoardCellSet& voidCells, const BoardCellSet& rollbackCells=BoardCellSet()) const;

    const static uint_fast32_t GUESS_COUNT = 1;
};

} // namespace EGM

#endif /* NURIKABE_SOLVER_H_ */


