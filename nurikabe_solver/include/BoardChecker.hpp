#ifndef BOARD_CHECKER_H_
#define BOARD_CHECKER_H_

#include <common.hpp>
#include <IndexedBoard.hpp>

namespace EGM {

enum class CheckEffort { LOW, MEDIUM, HIGH};
enum MoveType { VOID_TO_RED=1, BLUE_TO_RED=2, RED_TO_BLUE=4, VOID_TO_BLUE=5 };

class BoardChecker {
  public:
    BoardChecker(IndexedBoard* idxBoard);

    bool checkCompletelyFullBoard() const;
    template <CheckEffort Effort=CheckEffort::HIGH>
    bool checkNextMoveWithEffort(const BoardCell& iCell, int_type iNextValue);

    string toString() const;

  private:
    bool checkDeeplyFullRedBoard() const;

    template <CheckEffort Effort>
    bool checkNextMoveWithEffortHelper(const BoardCell& iCell, MoveType iType);

    template <class Iterator, int_fast32_t CountOffset>
    bool checkDeeplyEmptyOrFullBlueBoard(const BoardCell& iStartCell) const;
    template <class Iterator, int_fast32_t CountOffset>
    bool checkQuicklyEmptyOrFullBlueBoard(const BoardCell& iStartCell) const;

    bool checkDeeplyNextMoveRed(const BoardCell& iStartCell) const;
    bool checkQuicklyNextMoveRed(const BoardCell& iStartCell) const;

    bool checkQuicklyNumberCell(const BoardCell& iStartCell) const;
    bool checkQuicklyCanReachNumberCell(const BoardCell& iStartCell) const;

    template <class Iterator, int_fast32_t CountOffset>
    bool checkQuicklySurroundingBlueCells(const BoardCell& iStartCell) const;
    template <class Iterator>
    bool checkQuicklySurroundingRedCells(const BoardCell& iStartCell) const;

    IndexedBoard* _idxBoard;
};

} // namespace EGM

#endif /* BOARD_CHECKER_H_ */

