#ifndef BOARD_CELL_H_
#define BOARD_CELL_H_

#include <common.hpp>
#include <BaseBoard.hpp>

namespace EGM {

class BoardCell {
  public:
    BoardCell() = default;
    BoardCell(const BoardCell& iBoard) = default;
    BoardCell(uint_fast32_t x, uint_fast32_t y) : _x(x), _y(y) {}

    uint_fast32_t x() const { return _x; }
    uint_fast32_t y() const { return _y; }
    void x(uint_fast32_t x) { _x = x; }
    void y(uint_fast32_t y) { _y = y; }

    int_type value(const BaseBoard& iBoard) const;  
    bool isCoordOk (const BaseBoard& iBoard) const; 

    bool operator< (const BoardCell& other) const;
    bool operator== (const BoardCell& other) const;
    bool operator!= (const BoardCell& other) const;

    void move(uint_fast32_t dx, uint_fast32_t dy) { x( x() + dx ), y( y() + dy ); }
    void move(const int_fast32_t* dxy) { x( x() + *dxy ), y( y() + *(dxy+1) ); }

    string toString() const;

  private:
    uint32_t _x, _y;
};

inline bool BoardCell::operator< (const BoardCell& other) const {
  return y() < other.y() || ( y() == other.y() && x() < other.x() );
}

inline bool BoardCell::operator== (const BoardCell& other) const {
  return y() == other.y() && x() == other.x();
}

inline bool BoardCell::operator!= (const BoardCell& other) const {
  return y() != other.y() || x() != other.x();
}

inline int_type BoardCell::value(const BaseBoard& iBoard) const { 
  ASSERT(isCoordOk(iBoard), "Called value on invalid cell " << toString()); 
  return iBoard.get(x(), y()); 
}

inline bool BoardCell::isCoordOk (const BaseBoard& iBoard) const { 
  return (x() < iBoard.xlen() && y() < iBoard.ylen()); 
}

} // namespace EGM

#endif /* BOARD_CELL_H_ */

