/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#ifndef PROGRAMGENERATOR_H_
#define PROGRAMGENERATOR_H_

#include "logging.h"
#include "ProgramState.h"
#include "ProgramMap.h"
#include <random>

/*
 * Generates program source code (as program states with ip = 0).
 * I would have liked to use polymorphism to implement generators using different policies but I do
 * not want to incur the virtual function penalty so I just create several classes with the same
 * interface ...
 */
class BaseGenerator {
  public:
    ProgramState nextProgram();
    bool hasNext() const;
    std::string perfInformation() const;

    /**
     * Discards programs guarantee not to be busy beavers. Only quick checks applied.
     */
    bool isBadCandidate(const ProgramState& iCandidate);

  protected:
    BaseGenerator();
    void decrementToGoCount();
    std::string toStringBase() const;

    unsigned minLen, maxLen;
    BigInteger toGoCount, doneCount, filterCount, failedGen;
    uint32_t instUsedInProgBitSet, forbiddenInstBitSet;
    bool activateCandidateFiltering;
};

/**
 * Generates the full set of possible programs.
 */
class CompleteGenerator : public BaseGenerator {
  public:
    CompleteGenerator();
    ProgramState nextProgram();

    friend std::ostream& operator <<(std::ostream& oOs, const CompleteGenerator& iItem);

  protected:
    uint8_t lastGenerated[MAX_CODE_LEN];
};

/**
 * Returns randomly generated programs, it does not guarantee full coverage of problem space.
 */
class RandomGenerator : public BaseGenerator {
  public:
    RandomGenerator();
    ProgramState nextProgram();

    friend std::ostream& operator <<(std::ostream& oOs, const RandomGenerator& iItem);

  protected:
    const static unsigned GIVE_UP_THRESHOLD = 16;
    const static uint64_t GEN_MASK = 0x0f0f0f0f0f0f0f0f, GEN_LEN = 8;
    std::mt19937_64 numberGenerator;
    ProgramMap alreadyGenerated;
};

#endif /* PROGRAMGENERATOR_H_ */
