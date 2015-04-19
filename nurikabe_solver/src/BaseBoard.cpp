#include <cstdlib>
#include <iomanip>
#include <boost/regex.hpp>
#include <fstream>
#include <BaseBoard.hpp>

namespace EGM {

template<>
string BaseBoard::toString () const {
  stringstream os;
  uint_fast32_t count=0;

  for (auto it=begin(); it!=end(); ++it, ++count) {
    const char* color = COL_RESET;
    if (*it != Cell::VOID) 
      color = *it == Cell::BLUE ? COL_BLUE : COL_RED;

    if (count % xlen() == 0) os << endl;
    os << color << setw(3) << Cell::toString(*it);
  }
  os << COL_RESET;
  return os.str();
}

template<>
string BaseBoard::serialize () const {
  stringstream os;
  ostream_iterator<uint_fast32_t> oIt(os, ",");
  copy(begin(), end(), oIt);
  return os.str();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int_type Cell::fromString(const string& str) {
  if (str == "V" || str == "VOID" || str == "void")
    return Cell::VOID;
  if (str == "R" || str == "RED" || str == "red")
    return Cell::RED;
  if (str == "B" || str == "BLUE" || str == "blue")
    return Cell::BLUE;

  int_type result = stoi(str);
  ASSERT(VoidPredicate(result) || RedPredicate(result) || BluePredicate(result), "Unknown cell value " << str);
  return result;
}

string Cell::toString(int_type value) {
  switch (value) {
    case Cell::VOID : return "0";
    case Cell::BLUE : return "B";
    case Cell::RED : return "R";
    default: return to_string(value);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<>
string BoardMask::toString () const {
  stringstream os;

  for (uint_fast32_t y=0; y < ylen(); ++y)
    for (uint_fast32_t x=0; x < xlen(); ++x) {
      if (x % xlen() == 0) os << endl;
      os << setw(2) << _mask.test(x+y*ylen());
    }
  return os.str();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void readBoardFromInStream (istream& iStream, BaseBoard& oBoard) {
  BaseBoard::iterator boardIt = oBoard.begin();
  boost::regex numberRx("\\w+");
  boost::smatch matchResult;

  for (string line; getline(iStream, line) && boardIt != oBoard.end(); line.clear()) {
    //LOG_DEBUG("Read line : " << line);
    auto stringPos = line.cbegin();

    while ( regex_search(stringPos, line.cend(), matchResult, numberRx) && boardIt != oBoard.end() ) {
      stringPos += matchResult.position() + matchResult.length(0);
      *boardIt++ = Cell::fromString(matchResult.str());
    }
  }
  ASSERT(boardIt == oBoard.end(), "Input Stream too short!");
}

void readBoardFromFile (string iFileName, BaseBoard& oBoard) {
  ifstream inFile(iFileName);
  ASSERT(inFile, "Could not open file " << iFileName << " for reading");
  readBoardFromInStream(inFile, oBoard);
}

void readBoardFromString (const string& iStrBoard, BaseBoard& oBoard) {
  ASSERT(iStrBoard.size(), "String is too short");
  stringstream is(iStrBoard);
  readBoardFromInStream(is, oBoard);
}

} // namespace EGM

