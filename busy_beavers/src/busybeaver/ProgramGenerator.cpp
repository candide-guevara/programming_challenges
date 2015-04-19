/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#include "ProgramGenerator.h"
#include <cstring>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <chrono>
#include <algorithm>

const static uint32_t BIT_SET[MAX_CODE_LEN] = {
  [0] = 1, // I_HALT
  [1] = 1 << 1,// I_INC_R0
  [2] = 1 << 2,// I_INC_R1
  [3] = 1 << 3,// I_DEC_R0
  [4] = 1 << 4,// I_DEC_R1
  [5] = 1 << 5,// I_SUM_R1_TO_R0
  [6] = 1 << 6,// I_SUB_R1_TO_R0
  [7] = 1 << 7,// I_PRINT
  [8] = 1 << 8,// I_JMP_NOT_NULL
  [9] = 1 << 9,// I_JMP_NULL
  [10] = 1 << 10,// I_ABS_TO_R0
  [11] = 1 << 11,// I_ABS_TO_R1
  [12] = 1 << 12,// I_MOV_R0_TO_ADD
  [13] = 1 << 13,// I_MOV_R1_TO_ADD
  [14] = 1 << 14,// I_SWP_R0_ADD
  [15] = 1 << 15// I_SWP_R1_ADD
};

BaseGenerator::BaseGenerator() {
  const Config& conf = *Config::instance();

  ASSERT(sizeof(uint32_t) * 8 > MAX_VALUE, "Instruction set length greater than bit set");
  ASSERT(conf.minProgramLen < conf.maxProgramLen, "Minimum/maximum program lengths are wrong");
  ASSERT(conf.maxProgramLen <= MAX_CODE_LEN, "Cannot generate programs longer than " << MAX_CODE_LEN);

  minLen = conf.minProgramLen;
  maxLen = conf.maxProgramLen;
  toGoCount = conf.maxProgramPropositions ? conf.maxProgramPropositions : MAX_BIG_INT;
  activateCandidateFiltering = conf.activateCandidateFiltering;
  forbiddenInstBitSet = conf.forbiddenInstructions;

  failedGen = doneCount = filterCount = 0;
}

bool BaseGenerator::hasNext() const {
  return toGoCount > 0;
}
void BaseGenerator::decrementToGoCount() {
  toGoCount -= toGoCount ? 1 : 0;
  INC_PERF_COUNTER(doneCount);
}

bool BaseGenerator::isBadCandidate(const ProgramState& iCandidate) {
  const static uint32_t MOV_TO_ADD_INST = BIT_SET[I_MOV_R0_TO_ADD] | BIT_SET[I_MOV_R1_TO_ADD] | BIT_SET[I_SWP_R0_ADD]
      | BIT_SET[I_SWP_R1_ADD];
  const static uint32_t JMP_INST = BIT_SET[I_JMP_NULL] | BIT_SET[I_JMP_NOT_NULL];

  if (!activateCandidateFiltering)
    return false;

  // 0. The program starts with the halt instruction
  if (iCandidate.getCode(0) == I_HALT)
    return true;

  // 1. We determine the instructions used in the program
  instUsedInProgBitSet = 0;
  for (unsigned idx = 0; idx < maxLen; ++idx)
    instUsedInProgBitSet |= BIT_SET[iCandidate.getCode(idx)];

  // 2. the program will not modify its code and it does not jump or print
  if (!(instUsedInProgBitSet & MOV_TO_ADD_INST))
    if (!(instUsedInProgBitSet & JMP_INST) || !(instUsedInProgBitSet & BIT_SET[I_PRINT])) {
      INC_PERF_COUNTER(filterCount);
      return true;
    }

  // 3. If we find a forbidden instruction drop this program
  if (instUsedInProgBitSet & forbiddenInstBitSet) {
    INC_PERF_COUNTER(filterCount);
    return true;
  }

  LOG_DEBUG("Program got accepted, instructions : " << std::hex << instUsedInProgBitSet << std::dec);
  return false;
}

std::string BaseGenerator::perfInformation() const {
  std::ostringstream os;
  BigInteger donePercentage = 100*doneCount / (toGoCount + doneCount);
  os << "GeneratorPerf (covered=" << donePercentage << "%, generated=" << doneCount
      << ", filter_count=" << filterCount
      << ", failed_gen=" << failedGen << ")";
  return os.str();
}

std::string BaseGenerator::toStringBase() const {
  std::ostringstream repr;
  repr << "(minLen=" << minLen << ", maxLen=" << maxLen
      << ", filter=" << activateCandidateFiltering << ", to_go=" << toGoCount << std::hex
      << ", used_inst=" << instUsedInProgBitSet << ", forbid_inst=" << forbiddenInstBitSet
      << std::dec << ")";
  return repr.str();
}

CompleteGenerator::CompleteGenerator() :
    BaseGenerator() {
  // set all to 0 except the minLen bit to 1
  std::memset(lastGenerated, I_HALT, MAX_CODE_LEN);
  lastGenerated[minLen] = 1;

  // We calculate the max number of possible program and we take the max with configuration
  BigInteger programSpace = 0;
  ASSERT(MAX_CODE_LEN * VALUE_LEN <= sizeof(BigInteger) * 8, "Possible programs number is beyond our current precision");

  if (maxLen == MAX_CODE_LEN)
    programSpace = MAX_BIG_INT - bigIntPow(MAX_VALUE, minLen);
  else
    programSpace = bigIntPow(MAX_VALUE, maxLen) - bigIntPow(MAX_VALUE, minLen);

  LOG_INFO("Max program space : " << programSpace << ", configuration : " << toGoCount);
  toGoCount = toGoCount > programSpace ? programSpace : toGoCount;
}

ProgramState CompleteGenerator::nextProgram() {
  ASSERT(toGoCount > 0, "Cannot generate more programs");
  ProgramState result;
  result.resetRegisters();

  do {
    // Do not increment anything the first time (doneCount == 0)
    for (uint32_t idx=0, carry=1; doneCount && carry && idx < maxLen; ++idx) {
      lastGenerated[idx] = (lastGenerated[idx] + carry) % MAX_VALUE;
      carry = lastGenerated[idx] ? 0 : 1;
    }

    // TODO optim do not copy into program code, directly manipulate inner state
    result.loadCode(lastGenerated);
    decrementToGoCount();
    ASSERT(maxLen >= MAX_CODE_LEN || lastGenerated[maxLen] == 0, "Cannot generate anymore");
  }
  while (isBadCandidate(result));

  LOG_DEBUG("Generated : " << result << "   " << toGoCount);
  return result;
}

std::ostream& operator <<(std::ostream& oOs, const CompleteGenerator& iItem) {
  ProgramState curProg;
  curProg.loadCode(iItem.lastGenerated);
  oOs << "CompleteGenerator (" << iItem.toStringBase() << std::endl
      << "  last_prog=" << curProg << std::endl << ") // CompleteGenerator";
  return oOs;
}

RandomGenerator::RandomGenerator() :
    BaseGenerator() {
  ASSERT(MAX_CODE_LEN % GEN_LEN == 0, "Bad parameters " << MAX_CODE_LEN << " and " << GEN_LEN);
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  numberGenerator.seed(seed);
  alreadyGenerated.maxSizeHint(toGoCount);
}

ProgramState RandomGenerator::nextProgram() {
  ASSERT(toGoCount, "Cannot generate more programs");
  uint8_t code[MAX_CODE_LEN + 1];
  uint8_t *start=code+minLen, *end=code+maxLen;
  uint32_t giveUpCount = 0, maxZeroCount=maxLen-minLen, deleteLen=MAX_CODE_LEN-maxLen;
  bool isDuplicate, isTooShort;
  ProgramState result;
  result.resetRegisters();

  do {
    for (uint32_t idx = 0; idx < MAX_CODE_LEN; idx+=GEN_LEN)
      *((uint64_t*)(code+idx)) = numberGenerator() & GEN_MASK;

    isTooShort = std::count(start, end, I_HALT) >= maxZeroCount;
    if (isTooShort) continue;

    std::memset(end, I_HALT, deleteLen);
    result.loadCode(code);
    isDuplicate = alreadyGenerated.contains(result);

    if (!isDuplicate) {
      alreadyGenerated.addCopy(result);
    }
    else {
      if (giveUpCount & GIVE_UP_THRESHOLD) {
        LOG_DEBUG("After " << giveUpCount << " tries we cannot generate a new program " << result);
        failedGen++;
        break;
      }
      giveUpCount++;
    }
  }
  while (isDuplicate || isTooShort || isBadCandidate(result));

  LOG_DEBUG("Generated : " << result);
  decrementToGoCount();
  return result;
}

std::ostream& operator <<(std::ostream& oOs, const RandomGenerator& iItem) {
  oOs << "RandomGenerator (" << iItem.toStringBase() << std::endl
      << "  past_prog=" << iItem.alreadyGenerated << std::endl << ") // RandomGenerator";
  return oOs;
}
