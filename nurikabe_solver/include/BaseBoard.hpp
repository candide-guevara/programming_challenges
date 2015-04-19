#ifndef BASE_BOARD_H_
#define BASE_BOARD_H_

#include <vector>
#include <bitset>
#include <common.hpp>

namespace EGM {

typedef uint8_t int_type;

const static uint_fast32_t X_LEN = 9;
const static uint_fast32_t Y_LEN = 9;

struct Cell {
  enum { VOID=0, MAX_NUMBER=127, RED=128, BLUE, INVALID_MAX_NUMBER }; 
  static int_type invert(int_type col) { return col == RED ? BLUE : RED; }
  static int_type fromString(const string&);
  static string toString(int_type);
};

typedef bool (*Predicate_t)(int_type);
inline bool VoidPredicate(int_type iColor) { return Cell::VOID == iColor; }
inline bool BluePredicate(int_type iColor) { return Cell::BLUE == iColor; }
inline bool RedPredicate(int_type iColor) { return Cell::VOID != iColor && iColor <= Cell::RED; }
inline bool NumberCellPredicate(int_type iColor) { return Cell::VOID != iColor && iColor < Cell::RED; }
inline bool BlueOrVoidPredicate(int_type iColor) { return Cell::BLUE == iColor || Cell::VOID == iColor; }
inline bool RedOrVoidPredicate(int_type iColor) { return iColor <= Cell::RED; }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <uint_fast32_t Xlen, uint_fast32_t Ylen>
class BaseBoardTemplate {
  public:
    BaseBoardTemplate(): _board(Xlen * Ylen) {}

    int_type get(uint_fast32_t x, uint_fast32_t y) const {
      ASSERT(x < Xlen && y < Ylen, "Invalid indexes " << x << "," << y);
      return _board[x + y*Xlen];
    }
    void set(uint_fast32_t x, uint_fast32_t y, int_type value) {
      ASSERT(x < Xlen && y < Ylen, "Invalid indexes " << x << "," << y);
      ASSERT(value < Cell::INVALID_MAX_NUMBER, "Invalid value for cell");
      _board[x + y*Xlen] = value;
    }

    uint_fast32_t xlen() const { return Xlen; }
    uint_fast32_t ylen() const { return Ylen; }
    uint_fast32_t cellCount() const { return Ylen*Xlen; }

    typedef vector<int_type>::iterator iterator;
    typedef vector<int_type>::const_iterator const_iterator;
    iterator begin()             { return _board.begin(); }
    iterator end()               { return _board.end(); }
    const_iterator begin() const { return _board.begin(); }
    const_iterator end() const   { return _board.end(); }

    bool operator== (const BaseBoardTemplate<Xlen,Ylen>& other) { return other._board == _board; }

    string toString() const;
    string serialize() const;

  private:
    vector<int_type> _board;
};


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <uint_fast32_t Xlen, uint_fast32_t Ylen>
class BoardMaskTemplate {
  public:
    bool testAndSet(uint_fast32_t x, uint_fast32_t y); 

    uint_fast32_t xlen() const { return Xlen; }
    uint_fast32_t ylen() const { return Ylen; }

    void reset() { _mask.reset(); }
    string toString() const;

  private:
    bitset<Xlen*Ylen> _mask;
};

// Handy shortcut to avoid mentioning the size of the board each time
typedef BaseBoardTemplate<X_LEN, Y_LEN> BaseBoard;
typedef BoardMaskTemplate<X_LEN, Y_LEN> BoardMask;

template <uint_fast32_t Xlen, uint_fast32_t Ylen>
ostream& operator << (ostream& os, const BaseBoardTemplate<Xlen,Ylen>& iBoard) {
  os << iBoard.toString();
  return os;
}

template <uint_fast32_t Xlen, uint_fast32_t Ylen>
bool BoardMaskTemplate<Xlen, Ylen>::testAndSet(uint_fast32_t x, uint_fast32_t y) {
  ASSERT(x < Xlen && y < Ylen, "Invalid indexes " << x << "," << y);
  bool result = _mask[x + y*Xlen];
  _mask[x + y*Xlen] = true;
  return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void readBoardFromInStream (istream& iStream, BaseBoard& oBoard);
void readBoardFromString (const string& iStrBoard, BaseBoard& oBoard);
void readBoardFromFile (string iFileName, BaseBoard& oBoard);

} // namespace EGM

#endif /* BASE_BOARD_H_ */
