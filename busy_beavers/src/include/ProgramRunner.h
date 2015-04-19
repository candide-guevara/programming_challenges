/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#ifndef PROGRAMRUNNER_H_
#define PROGRAMRUNNER_H_

#include "logging.h"
#include <ProgramMap.h>
#include <ProgramToSave.h>
#include "StepForward.h"

// Backdoors for tests ...
namespace TST {
  void TestHaltingPrograms();
  void TestShortLoopingPrograms();
  void TestLongLoopingPrograms();
}  // namespace TST

/**
 * Runs a program until it stops or we guess it will never halt. You should check from time to
 * time that the states stored in memory are not too big.
 */
class ProgramRunner {
  public:
    ProgramRunner();

    void checkIfItHalts(const ProgramState& iInitState, ExeStats& oEndStats);
    /**
     * This function is meant to lighten the memory footprint by giving up half of the stored states.
     * You will not run out of memory but you will be slower to determine the program outcomes.
     */
    void dropHalfOfStoredStates();
    std::string perfInformation() const;

    friend std::ostream& operator <<(std::ostream& oOs, const ProgramRunner& iItem);
    friend void TST::TestHaltingPrograms();
    friend void TST::TestShortLoopingPrograms();
    friend void TST::TestLongLoopingPrograms();

  private:
    void runAProgramWeHaveNotSeenBefore(const ProgramState& iInitState, ExeStats& oEndStats);
    /**
     * This is the heart of the algorithm combining but emulation of the program and the
     * halting function logic.
     */
    void runUntilHaltsOrMaxIt();
    ProgramMap previousStates, pastPrograms;
    ProgramToSave statesToSave;
    StepForward emulator;
    BigInteger tryUpToIterations, memLimit;
    BigInteger cacheHitCount, previousStateHitCount, longestRunning;
};

#endif /* PROGRAMRUNNER_H_ */
