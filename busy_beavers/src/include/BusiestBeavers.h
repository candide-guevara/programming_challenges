/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#ifndef BUSIESTBEAVERS_H_
#define BUSIESTBEAVERS_H_

#include "ProgramState.h"
#include "ExeStats.h"
#include <set>
#include <utility>

// clumsy way to avoid name conflicts ...
namespace Inner {
  typedef std::pair<ProgramState, ExeStats> Type;
}

class CompareOutput {
  public:
    bool operator ()(const Inner::Type &iItemL, const Inner::Type &iItemR) const {
      return iItemL.second.output < iItemR.second.output;
    }
};

/**
 * Fixed size priority queue to hold the busiest programs. The container will NOT check for duplicate programs at insertion.
 */
class BusiestBeavers: private std::multiset<Inner::Type, CompareOutput> {
  public:
    BusiestBeavers() :
        maxSize(0), weakestScore(0) {
    }

    void maxSizeHint(const BigInteger& iMaxSize) {
      this->maxSize = iMaxSize;
    }
    std::multiset<Inner::Type>::const_iterator begin() const {
      return cbegin();
    }
    std::multiset<Inner::Type>::const_iterator end() const {
      return cend();
    }
    BigInteger size() const {
      return std::multiset<Inner::Type, CompareOutput>::size();
    }
    const Inner::Type& busiest() const {
      return *crbegin();
    }
    void pushCopy(const ProgramState& iProg, const ExeStats& iStat);

    friend std::ostream& operator <<(std::ostream& oOs, const BusiestBeavers& iItem);

  private:
    BigInteger maxSize, weakestScore;
};

#endif /* BUSIESTBEAVERS_H_ */
