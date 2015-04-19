#include <sstream>
#include <BoardCell.hpp>

namespace EGM {

string BoardCell::toString() const {
  stringstream os;
  os << "(" << x() << "," << y() << ")";
  return os.str();
}

} // namespace EGM

