/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#ifndef BUSYBEAVERFINDER_H_
#define BUSYBEAVERFINDER_H_

#include "ProgramRunner.h"
#include "ProgramGenerator.h"
#include "BusiestBeavers.h"
#include "logging.h"

/**
 * Explores the space of program source code to guess the busiest beaver.
 */
class BusyBeaverFinder {
  public:
    BusyBeaverFinder();
    void determineTopPrograms ();
    const BusiestBeavers& getResults();

    std::string perfInformation() const;
    friend std::ostream& operator <<(std::ostream& oOs, const BusyBeaverFinder& iItem);

  private:
    void progressIndicator();

    const static unsigned POST_PROGRESS_EVERY = 1 << 18;
    // cannot use polymorphism since no of base generator methods are virtual ...
    CompleteGenerator generator;
    BusiestBeavers topPrograms;
    ProgramRunner runner;
    BigInteger undeterminedCount;
    BigInteger countToPrintProgress;
};

#endif /* BUSYBEAVERFINDER_H_ */
