#ifndef RANDOM_COLOR_SEQ_H_
#define RANDOM_COLOR_SEQ_H_

#include <random>
#include <chrono>
#include <BaseBoard.hpp>

namespace EGM {

template <class Engine, uint_fast32_t Len, uint_fast32_t Bias>
class RandomColorSeq {
  public:
    RandomColorSeq() { reinit(); }

    void reinit() {
      _state = 0;
      //unsigned seed_time = std::chrono::system_clock::now().time_since_epoch().count();
      //_engine.seed(seed_time);

      generate (_randomBuffer.begin(), _randomBuffer.end(), [=]() {
        uint64_t raw = this->_engine();
        return ((raw % 1024) < Bias) ? Cell::RED : Cell::BLUE;
      });

      //LOG_DEBUG("After initialization of sequence : " << toString());
    }

    int_type nextColor() {
      return _randomBuffer[_state++ % Len];
    }

    string toString() const {
      stringstream os;
      os << "RandomColorSeq (gen=" << typeid(Engine).name() << ", len=" << Len
        << ", bias=" << Bias << ")" << endl;
      os << "Sequence : " << toStrHelper(_randomBuffer) << endl;  
      return os.str();  
    }
  
  private:
    Engine _engine;
    array<uint_fast32_t, Len> _randomBuffer;
    uint_fast32_t _state;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <bool activate>
struct MyRandomHash { 
  MyRandomHash() {
    static uint_fast32_t CURSOR = 0;
    //const static array<uint64_t,16> PRIMES = {{ 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997 }};
    //const static array<uint64_t,16> PRIMES = {{ 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71 }};
    const static array<uint64_t,16> PRIMES = {{ 241, 2647, 8053, 1123, 3697, 9697, 4073, 7177, 2719, 1579, 4909, 3, 727, 491, 5939, 6959 }};
    _offset = PRIMES[ ++CURSOR % PRIMES.size() ];
  }  

  size_t operator() (const BoardCell& iCell) const {
    size_t result;
    if (activate)
      result = _hasher( (iCell.x() << 17) + iCell.y() * _offset);
    else   
      result = _hasher( (iCell.x() << 16) + iCell.y() );
    //LOG_DEBUG("offset " << _offset << " * " << iCell.toString() << " = " << result);
    return result;
  } 

  hash<uint64_t> _hasher;
  uint64_t _offset;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef RandomColorSeq<mt19937, 128, 384> MyRandomColorSeq;
typedef unordered_set<BoardCell,MyRandomHash<false> > RandomBoardCellSet;

} // namespace EGM

#endif /* RANDOM_COLOR_SEQ_H_ */

