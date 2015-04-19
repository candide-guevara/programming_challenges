/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#ifndef EXESTATS_H_
#define EXESTATS_H_

#include "logging.h"
#include "BigDecimal.h"

/**
 * Holds intersting information about a program execution.
 */
class ExeStats {
  public:
    ExeStats(): iterations(0), output(0), stops(false), reachedMaxIt(false) {}
    ExeStats(const BigInteger iIterations, const BigInteger iOutput, const bool iStops, const bool iReachedMaxIt);

    void reset();

    BigInteger iterations, output;
    bool stops, reachedMaxIt;
};

std::ostream& operator << (std::ostream& oOs, const ExeStats& iExe);

#endif /* EXESTATS_H_ */
