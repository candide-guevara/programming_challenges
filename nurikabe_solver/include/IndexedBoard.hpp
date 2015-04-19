#ifndef INDEXED_BOARD_H_
#define INDEXED_BOARD_H_

#include <unordered_set>
#include <common.hpp>
#include <CellIterators.hpp>

namespace EGM {

struct MyHash { 
  size_t operator() (const BoardCell& iCell) const {
    return _hasher( (iCell.x() << 16) + iCell.y() );
  } 
  hash<uint_fast32_t> _hasher;
};
typedef unordered_set<BoardCell,MyHash> BoardCellSet;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define FORWARD_COLLECTION(cont_type, cell_type) \
  typedef cont_type::const_iterator cell_type##it_t; \
  cell_type##it_t cell_type##Begin() const { return _##cell_type##Cells.cbegin(); } \
  cell_type##it_t cell_type##End()   const { return _##cell_type##Cells.cend(); } \
  uint_fast32_t   cell_type##Size()  const { return _##cell_type##Cells.size(); }

class IndexedBoard {
  public:
    IndexedBoard(const BaseBoard& iBoard);
    IndexedBoard(const string& iSerializedBoard);
    IndexedBoard(const IndexedBoard& iOther) = delete;

    void buildIndexes();

    int_type get(uint_fast32_t x, uint_fast32_t y) const { return _board.get(x,y); }
    int_type get(const BoardCell& iCell) const { return get(iCell.x(), iCell.y()); }

    void set(uint_fast32_t x, uint_fast32_t y, int_type value) { set(BoardCell(x,y), value); }
    void set(const BoardCell& iCell, int_type value);

    const BaseBoard& cboard() const { return _board; }
    BaseBoard& board() { return _board; }

    FORWARD_COLLECTION(vector<BoardCell>, number)
    FORWARD_COLLECTION(BoardCellSet, blue)
    FORWARD_COLLECTION(BoardCellSet, red)
    FORWARD_COLLECTION(BoardCellSet, void)

    uint_fast32_t maxRedCount() const { return _maxRedCount; }
    uint_fast32_t maxBlueCount() const { return _maxBlueCount; }

    string toString() const;

  private:
    BaseBoard _board;
    vector<BoardCell> _numberCells;

    BoardCellSet _blueCells;
    BoardCellSet _redCells;
    BoardCellSet _voidCells;

    uint_fast32_t _maxRedCount;
    uint_fast32_t _maxBlueCount;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

uint_fast32_t countConnectedByColor (const BoardCell& iCell, const BaseBoard& iBoard);
uint_fast32_t countConnectedByColor (const BoardCell& iCell, const BaseBoard& iBoard, Predicate_t predicate);

} // namespace EGM

#endif /* INDEXED_BOARD_H_ */


