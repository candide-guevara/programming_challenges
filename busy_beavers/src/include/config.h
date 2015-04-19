/*
 * Created on: Nov 23, 2013
 *      Author: eguevara
 */

#ifndef CONFIG_H_
#define CONFIG_H_

#include "logging.h"
#include "BigDecimal.h"

const unsigned VALUE_LEN = 4;
const unsigned MAX_CODE_LEN = 16;
const unsigned MAX_VALUE = 16;

typedef enum {
  I_HALT = 0,
  I_INC_R0,
  I_INC_R1,
  I_DEC_R0,
  I_DEC_R1,
  I_SUM_R1_TO_R0,
  I_SUB_R1_TO_R0,
  I_PRINT,
  I_JMP_NOT_NULL,
  I_JMP_NULL,
  I_ABS_TO_R0,
  I_ABS_TO_R1,
  I_MOV_R0_TO_ADD,
  I_MOV_R1_TO_ADD,
  I_SWP_R0_ADD,
  I_SWP_R1_ADD
} Instructions;

class Config {
  private:
    Config() {}

  public:
    static Config* instance()  {
      static Config* singleton = new Config;
      return singleton;
    }
    void loadOptions(int argc, char **argv);
    void setDefaults();
    void setDefaultForTest();
    uint32_t parseBitSet (const char *iStr) const;

    BigInteger maxProgramIteration, maxInMemoryPrograms;
    BigInteger saveCountPerRun, saveDropRatio;
    BigInteger maxProgramPropositions;
    unsigned minProgramLen, maxProgramLen;
    unsigned topProgramCount;
    bool activateCandidateFiltering, runTestSuite;
    uint32_t forbiddenInstructions;
};

std::ostream& operator <<(std::ostream& oOs, const Config& iItem);
#endif /* CONFIG_H_ */
