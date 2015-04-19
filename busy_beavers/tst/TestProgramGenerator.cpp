/*
 * Created on: Nov 29, 2013
 *      Author: eguevara
 */

#include <ProgramGenerator.h>
#include <unittest.h>
#include <config.h>
#include <cmath>

namespace TST {

  void testCandidateFiltering() {
    ::Config& conf = *::Config::instance();
    conf.activateCandidateFiltering = true;
    ProgramState prog;
    CompleteGenerator filter;

    uint8_t startWithHalt[20] = { I_HALT, I_PRINT, I_PRINT, I_PRINT, I_PRINT, I_PRINT, I_PRINT, I_PRINT, I_PRINT };
    uint8_t noPrintAndJmp[20] = { I_INC_R0, I_PRINT, I_INC_R0, I_INC_R0, I_INC_R0, I_INC_R0, I_INC_R0, I_INC_R0, I_INC_R0 };
    uint8_t noMovOrSwap[20] =
        { I_INC_R0, I_ABS_TO_R0, I_INC_R0, I_INC_R0, I_SUM_R1_TO_R0, I_INC_R0, I_DEC_R1, I_INC_R1, I_INC_R0 };
    uint8_t okPrintAndJmp[20] = { I_INC_R0, I_PRINT, I_INC_R0, I_JMP_NULL, I_INC_R0, I_INC_R0, I_INC_R0, I_INC_R0, I_INC_R0 };
    uint8_t okMovOrSwap[20] = { I_INC_R0, I_ABS_TO_R0, I_SWP_R0_ADD, I_INC_R0, I_SUM_R1_TO_R0, I_INC_R0, I_DEC_R1, I_INC_R1,
        I_INC_R0 };

    prog.loadCode(startWithHalt);
    TST_ASSERT(filter.isBadCandidate(prog), "Bad candidate : starts with halt instruction");
    prog.loadCode(noPrintAndJmp);
    TST_ASSERT(filter.isBadCandidate(prog), "Bad candidate : does not contain a jump and print instructions");
    prog.loadCode(noMovOrSwap);
    TST_ASSERT(filter.isBadCandidate(prog), "Bad candidate : does not contain swap or move instruction");
    prog.loadCode(okPrintAndJmp);
    TST_ASSERT(!filter.isBadCandidate(prog), "Good candidate : contains a jump and print instructions");
    prog.loadCode(okMovOrSwap);
    TST_ASSERT(!filter.isBadCandidate(prog), "Good candidate : contains swap or move instruction");
  }

  void TestFilteringByInstructions() {
    ::Config& conf = *::Config::instance();
    conf.activateCandidateFiltering = true;
    conf.maxProgramLen = 10;
    conf.forbiddenInstructions = conf.parseBitSet("0110");

    ProgramState prog;
    CompleteGenerator filter;
    uint8_t withForbiddenInst[20] = { I_INC_R0, I_PRINT, I_DEC_R1, I_DEC_R1, I_DEC_R1, I_DEC_R1, I_DEC_R1, I_DEC_R1, I_DEC_R1 };
    uint8_t withForbiddenInst2[20] = { I_PRINT, I_PRINT, I_DEC_R1, I_DEC_R1, I_INC_R1, I_DEC_R1, I_DEC_R1, I_DEC_R1, I_DEC_R1 };
    uint8_t onlyValidInst[20] = { I_PRINT, I_PRINT, I_DEC_R1, I_DEC_R1, I_SUM_R1_TO_R0, I_DEC_R0, I_DEC_R1, I_JMP_NULL, I_PRINT };

    prog.loadCode(withForbiddenInst);
    TST_ASSERT(filter.isBadCandidate(prog), "Bad candidate : contains forbidden instructions");
    prog.loadCode(withForbiddenInst2);
    TST_ASSERT(filter.isBadCandidate(prog), "Bad candidate : contains forbidden instructions (2)");
    prog.loadCode(onlyValidInst);
    TST_ASSERT(!filter.isBadCandidate(prog), "Good candidate : contains only valid instructions");
    conf.forbiddenInstructions = 0;
  }

  void testCompleteGenerator() {
    ::Config& conf = *::Config::instance();
    conf.activateCandidateFiltering = false;
    conf.minProgramLen = 5;
    conf.maxProgramLen = 8;
    CompleteGenerator generator;
    ProgramMap allProg;

    for (BigInteger i = 0; i < conf.maxProgramPropositions; ++i) {
      TST_ASSERT(generator.hasNext(), "Generator exhausted too soon");
      ProgramState prog = generator.nextProgram();
      allProg.addCopy(prog);
      TST_ASSERT(!prog.getR0() && !prog.getR1() && !prog.getIp(), "Bad initial register state");
      TST_ASSERT(!prog.getCode(conf.maxProgramLen), "Bad program length");
    }

    TST_ASSERT(allProg.size() == conf.maxProgramPropositions,
        "Generated duplicate programs " << conf.maxProgramPropositions - allProg.size());
    TST_ASSERT(!generator.hasNext(), "Generator does not respect configuration");
  }

  void testCompleteGeneratorBorderLineLengths() {
    ::Config& conf = *::Config::instance();
    conf.activateCandidateFiltering = false;
    conf.minProgramLen = 0;
    conf.maxProgramLen = 1;
    conf.maxProgramPropositions = MAX_VALUE - 1;

    ProgramState prog;
    CompleteGenerator generator;

    for (BigInteger i = 0; i < conf.maxProgramPropositions; ++i) {
      TST_ASSERT(generator.hasNext(), "Generator exhausted too soon");
      prog = generator.nextProgram();
    }
    TST_ASSERT(!generator.hasNext(), "Generator will generate more programs than the space size");
    TST_ASSERT(prog.getCode(0) == MAX_VALUE - 1, "The last program generated was not the expected");

    conf.minProgramLen = MAX_CODE_LEN-1;
    conf.maxProgramLen = MAX_CODE_LEN;
    conf.maxProgramPropositions = MAX_VALUE + 1;
    CompleteGenerator generator2;

    for (BigInteger i = 0; i < conf.maxProgramPropositions; ++i) {
      TST_ASSERT(generator2.hasNext(), "Generator exhausted too soon (2)");
      prog = generator2.nextProgram();
    }
    TST_ASSERT(!generator2.hasNext(), "Generator will generate more programs than the space size (2)");
    TST_ASSERT(prog.getCode(0) == 0 && prog.getCode(1) == 1 && prog.getCode(MAX_CODE_LEN-1) == 1,
        "The last program generated was not the expected (2) " << prog);
  }

  void testCompleteGeneratorAllProgramSpace() {
    ::Config& conf = *::Config::instance();
    conf.activateCandidateFiltering = false;
    conf.minProgramLen = 2;
    conf.maxProgramLen = 3;
    conf.maxProgramPropositions = 10000;
    BigInteger programSpace = std::pow(MAX_VALUE, conf.maxProgramLen) - std::pow(MAX_VALUE, conf.minProgramLen);

    ProgramState prog;
    CompleteGenerator generator;

    for (BigInteger i = 0; i < programSpace; ++i) {
      TST_ASSERT(generator.hasNext(), "Generator exhausted too soon");
      prog = generator.nextProgram();
    }
    TST_ASSERT(!generator.hasNext(), "Generator will generate more programs than the space size");

    bool goodLast = true;
    for (uint32_t idx = 0; idx < conf.maxProgramLen; ++idx)
      if (prog.getCode(idx) != MAX_VALUE - 1)
        goodLast = false;
    TST_ASSERT(goodLast, "The last program generated was not the expected " << prog);

    conf.maxProgramLen = 5; // programSpace = 1048320;
    CompleteGenerator generator2;

    for (BigInteger i = 0; i < conf.maxProgramPropositions; ++i) {
      TST_ASSERT(generator2.hasNext(), "Generator exhausted too soon (2)");
      prog = generator2.nextProgram();
    }
    TST_ASSERT(!generator2.hasNext(), "Generator will generate more programs than the space size (2)");

    uint32_t expectedProg = std::pow(MAX_VALUE,conf.minProgramLen) + conf.maxProgramPropositions - 1;
    uint32_t lastProg = 0;
    for (uint32_t idx=0; idx < MAX_CODE_LEN; ++idx)
      lastProg += prog.getCode(idx) * std::pow(MAX_VALUE, idx);
    TST_ASSERT(lastProg == expectedProg,
        "The last program generated was not the expected (2) " << lastProg << " / " << expectedProg);
  }

  void testRandomGenerator() {
    ::Config& conf = *::Config::instance();
    conf.minProgramLen = 4;
    conf.maxProgramLen = 10;
    conf.activateCandidateFiltering = true;
    RandomGenerator generator;
    ProgramMap allProg;

    for (BigInteger i = 0; i < conf.maxProgramPropositions; ++i) {
      TST_ASSERT(generator.hasNext(), "Generator exhausted too soon");
      ProgramState prog = generator.nextProgram();
      allProg.addCopy(prog);
      TST_ASSERT(!prog.getR0() && !prog.getR1() && !prog.getIp(), "Bad initial register state");
      // The candidate  program should not start by the halt instruction when filtering is on
      TST_ASSERT(prog.getCode(0) && !prog.getCode(conf.maxProgramLen), "Bad program length " << prog);
    }

    TST_ASSERT(allProg.size() == conf.maxProgramPropositions,
        "Generated duplicate programs " << conf.maxProgramPropositions - allProg.size());
    TST_ASSERT(!generator.hasNext(), "Generator does not respect configuration");
  }

  void testRandomGeneratorGenerateAll() {
    ::Config& conf = *::Config::instance();
    conf.minProgramLen = 2;
    conf.maxProgramLen = 4;
    conf.maxProgramPropositions = 128 * 1024;
    conf.activateCandidateFiltering = false;
    RandomGenerator generator;
    ProgramMap allProg;

    for (BigInteger i = 0; i < conf.maxProgramPropositions; ++i) {
      TST_ASSERT(generator.hasNext(), "Generator exhausted too soon");
      ProgramState prog = generator.nextProgram();
      allProg.addCopy(prog);
      TST_ASSERT(prog.getCode(2) || prog.getCode(3), "Bad program length " << prog);
    }
    TST_ASSERT(allProg.size() == 64 * 1024 - 256, "Could not generate the whole program space " << allProg.size());
  }

} /* namespace TST */

