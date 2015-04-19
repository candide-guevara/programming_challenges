#ifndef CELL_ITERATORS_H_
#define CELL_ITERATORS_H_

#include <vector>
#include <iterator>
#include <algorithm>
#include <common.hpp>
#include <BoardCell.hpp>

namespace EGM {

enum class IteratorType { CONNECTED, ADJACENT, POOL };

template <IteratorType Type>
class GenericIt : public iterator<input_iterator_tag, BoardCell> {
  public:
    static const int_fast32_t TRANSFORMATIONS[][2];
    static const uint_fast32_t TRANSFORMATIONS_LEN;
    static const GenericIt<Type> END_IT;

    GenericIt(const GenericIt<Type>& iIt) = default;
    GenericIt(const BoardCell& iCell, const BaseBoard* iBoard);

    bool operator== (const GenericIt<Type>& other) { return _state == other._state; }
    bool operator!= (const GenericIt<Type>& other) { return _state != other._state; }

    GenericIt<Type>& operator++ ();
    BoardCell& operator* () { return _current; }

    void reset(const BoardCell &iCell);

    int_type value() const { return _current.value(*_board); }
    const BaseBoard& board() const { return *_board; }

    string toString() const;
  
  private:
    GenericIt(uint_fast32_t state) : _state(state) {}

    uint_fast32_t _state;
    BoardCell _current;
    const BaseBoard* _board; 
};

template <IteratorType Type> const GenericIt<Type> GenericIt<Type>::END_IT(TRANSFORMATIONS_LEN);
typedef GenericIt<IteratorType::CONNECTED> ConnectedIt;
typedef GenericIt<IteratorType::ADJACENT> AdjacentIt;
typedef GenericIt<IteratorType::POOL> PoolIt;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <Predicate_t ColorPredicate, class CellIterator=ConnectedIt>
class ColorIt : public iterator<input_iterator_tag, BoardCell> {
  public:
    // depends on CellIterator::END_IT = be carefull about static init fiasco
    static const ColorIt<ColorPredicate,CellIterator> END_IT;

    ColorIt(const ColorIt<ColorPredicate,CellIterator>& iIt) = default;
    ColorIt(const BoardCell& iCell, const BaseBoard* iBoard);

    bool operator!= (const ColorIt<ColorPredicate>& other) { return _it != other._it; }
    bool operator== (const ColorIt<ColorPredicate>& other) { return !(*this == other); }

    ColorIt<ColorPredicate,CellIterator>& operator++ ();
    BoardCell& operator* () { return *_it; }

    const BaseBoard& board() const { return _it.board(); }
    int_type value() const { return _it.value(); }

    string toString() const;
    void reset(const BoardCell &iCell);

  private:
    ColorIt(CellIterator it) : _it(it) {}

    BoardMask _mask;
    vector<BoardCell> _stack;
    //array<BoardCell,81> _stack;
    //uint_fast32_t _level;
    CellIterator _it;
};

template <Predicate_t ColorPredicate, class CellIterator>
const ColorIt<ColorPredicate,CellIterator> ColorIt<ColorPredicate,CellIterator>::END_IT(CellIterator::END_IT);

typedef ColorIt<&VoidPredicate> VoidConnectedIt;
typedef ColorIt<&BluePredicate> BlueConnectedIt;
typedef ColorIt<&BlueOrVoidPredicate> BlueOrVoidConnectedIt;
typedef ColorIt<&RedPredicate> RedConnectedIt;
typedef ColorIt<&RedOrVoidPredicate> RedOrVoidConnectedIt;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <Predicate_t ColorPredicate, class CellIterator>
ColorIt<ColorPredicate,CellIterator>::ColorIt(const BoardCell& iCell, const BaseBoard* iBoard) 
    : /*_level(0),*/ _it(iCell, iBoard) { 
  STAT_COUNT(ColorIt::CopyCtor);
  //_stack.reserve(iBoard->cellCount());
  _mask.testAndSet(iCell.x(), iCell.y());
}
/*
template <Predicate_t ColorPredicate, class CellIterator>
ColorIt<ColorPredicate,CellIterator>::ColorIt(const ColorIt<ColorPredicate,CellIterator>& iIt) 
  : _mask(iIt._mask), _stack(iIt._stack), _it(iIt._it) {
  STAT_COUNT(ColorIt::CopyCtor);
}
*/
template <Predicate_t ColorPredicate, class CellIterator>
ColorIt<ColorPredicate,CellIterator>& ColorIt<ColorPredicate,CellIterator>::operator++() {
  STAT_START(ColorItIncrement);
  ASSERT(_it != CellIterator::END_IT, "Iterating over an exhausted iterator");

  for (++_it; _it != CellIterator::END_IT; ++_it) {
    const BoardCell& iCell = *_it;
    if (ColorPredicate( _it.value() ) && !_mask.testAndSet(iCell.x(), iCell.y()) ) {
      _stack.push_back(iCell);
      //_stack[_level++] = iCell;
    }  
  }

  //LOG_DEBUG("After iterating on neighbourgs " << toString());
  //if(_level) {
  if(!_stack.empty()) {
    _it.reset(_stack.back());
    _stack.pop_back();
    //_it.reset(_stack[--_level]);
  } 

  STAT_STOP(ColorItIncrement);
  return *this;
}

template <Predicate_t ColorPredicate, class CellIterator>
void ColorIt<ColorPredicate,CellIterator>::reset(const BoardCell& iCell) {
  ASSERT(ColorPredicate(iCell.value(board())), "invoked color iterator with wrong predicate ?");
  _mask.reset();
  _mask.testAndSet(iCell.x(), iCell.y());
  _stack.clear();
  //_level = 0;
  _it.reset(iCell);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*template <IteratorType Type>
GenericIt<Type>::GenericIt(const GenericIt<Type>& iIt) 
  : _state(iIt._state), _current(iIt._current), _board(iIt._board) {
  STAT_COUNT(GenericIt::CopyCtor);
  ASSERT(_board || _state == TRANSFORMATIONS_LEN, "Null inner board"); 
}
*/
template <IteratorType Type>
GenericIt<Type>::GenericIt(const BoardCell& iCell, const BaseBoard* iBoard) 
  : _state(), _current(iCell), _board(iBoard) { 
  STAT_COUNT(GenericIt::CopyCtor);
  ASSERT(_board || _state == TRANSFORMATIONS_LEN, "Null inner board"); 
}

template <IteratorType Type>
void GenericIt<Type>::reset(const BoardCell &iCell) {
  _current = iCell;
  _state = 0;
}

template <IteratorType Type>
GenericIt<Type>& GenericIt<Type>::operator++ () {
  STAT_START(GenericItIncrement);
  ASSERT(_state < TRANSFORMATIONS_LEN, "Iterating over an exhausted iterator");

  while ( ++_state != TRANSFORMATIONS_LEN ) {
    //_current.move(TRANSFORMATIONS[_state][0], TRANSFORMATIONS[_state][1]);
    _current.move(TRANSFORMATIONS[_state]);
    if (_current.isCoordOk(*_board)) break;
    //if (_current.x() < 9 && _current.y() < 9) break;
  }
  STAT_STOP(GenericItIncrement);
  return *this;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <IteratorType Type>
string GenericIt<Type>::toString() const {
  stringstream os;
  os << "[type=" << static_cast<uint_fast32_t>(Type) 
    << ",cell=" << _current.toString() 
    << ",board=" << _board << "]";
  return os.str();
}

template <Predicate_t ColorPredicate, class CellIterator>
string ColorIt<ColorPredicate,CellIterator>::toString() const {
  stringstream os;
  os << "[,stack=" << toStrHelper(_stack)
    << ",iterator=" << _it.toString()
    //<< ",mask=" << _mask.toString() 
    << "]";
  return os.str();
}

} // namespace EGM

#endif /* CELL_ITERATORS_H_ */
