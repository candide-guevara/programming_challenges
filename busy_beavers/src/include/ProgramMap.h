/*
 * Created on: Nov 23, 2013
 *      Author: eguevara
 */

#ifndef STATEMULTIMAP_H_
#define STATEMULTIMAP_H_

#include "logging.h"
#include "ProgramState.h"
#include <unordered_map>
#include "ExeStats.h"

/**
 * Contains a multi map assotiating program states to statistics about its execution.
 */
class ProgramMap {
    /**
     * Quick hash for a Program state to implement a multimap.
     */
    class StateHash {
      public:
        size_t operator() (const ProgramState& iState) const;
    };

    /**
     * Determines if 2 states are equal.
     */
    class StateEquals {
      public:
        bool operator() (const ProgramState& iStateL, const ProgramState& iStateR) const;
    };

    typedef std::unordered_map<ProgramState, ExeStats, StateHash, StateEquals> InnerMap;

  public:
    ProgramMap();

    void addCopy(const ProgramState& iKey);
    void addCopy(const ProgramState& iKey, const ExeStats& iValue);

    bool contains(const ProgramState& iKey);
    ExeStats* get(const ProgramState& iKey);

    void clear();
    BigInteger size() const;
    void maxSizeHint(const BigInteger imaxSize);
    std::string perfInformation() const;

    InnerMap::const_iterator begin();
    InnerMap::const_iterator end();

    // we need to access the inner container
    friend std::ostream& operator << (std::ostream& oOs, const ProgramMap& iMap);

  private:
    InnerMap states;
    double maxLoadFactor;
};

#endif /* STATEMULTIMAP_H_ */
