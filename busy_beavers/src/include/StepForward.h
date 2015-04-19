/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#ifndef STEPFORWARD_H_
#define STEPFORWARD_H_

#include "logging.h"
#include "ProgramState.h"
#include "ExeStats.h"

/**
 * Advances a program state to the next step.
 */
class StepForward {
  public:
    StepForward();

    void load(const ProgramState& newProgram, const BigInteger iMaxIteration);
    void advanceOnce();
    void executeInstruction(const Instructions iInst);

    const ProgramState& getCurProgram() const;
    const ExeStats& getCurStats() const;
    ExeStats& modifyCurStats();

    std::string perfInformation() const;
    // We print the inner fields
    friend std::ostream& operator <<(std::ostream& oOs, const StepForward& iItem);

  private:
    ProgramState curProgram;
    ExeStats curStats;
    BigInteger maxIteration, totalIterations;
};

#endif /* STEPFORWARD_H_ */
