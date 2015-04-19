/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#include <StepForward.h>

StepForward::StepForward() {
  totalIterations = 0;
}

void StepForward::load(const ProgramState& newProgram, const BigInteger iMaxIteration) {
  curProgram = newProgram;
  curStats.reset();
  maxIteration = iMaxIteration;
}

const ProgramState& StepForward::getCurProgram() const {
  return curProgram;
}
const ExeStats& StepForward::getCurStats() const {
  return curStats;
}
ExeStats& StepForward::modifyCurStats() {
  return curStats;
}

void StepForward::advanceOnce() {
  LOG_DEBUG("Before step : " << curProgram);
  ASSERT(!curStats.reachedMaxIt, "Cannot step forward, max iterations reached");

  executeInstruction(static_cast<Instructions>(curProgram.getCode()));
  curStats.iterations++;
  curStats.stops = curProgram.hasStopped();
  curStats.reachedMaxIt = curStats.iterations >= maxIteration;
  INC_PERF_COUNTER(totalIterations);

  LOG_DEBUG("After step : " << curProgram);
}

void StepForward::executeInstruction(const Instructions iInst) {
  uint8_t value, swap;
  unsigned nextIp = (curProgram.getIp() + 1) % MAX_VALUE;
  bool jump = false, doubleInc = false;

  // I hope the compiler does a jump table here
  switch (iInst) {
    case I_HALT:
      return;
    case I_ABS_TO_R0:
      doubleInc = true;
      value = curProgram.getCode(nextIp);
      curProgram.setR0(value);
      break;
    case I_ABS_TO_R1:
      doubleInc = true;
      value = curProgram.getCode(nextIp);
      curProgram.setR1(value);
      break;
    case I_DEC_R0:
      value = (curProgram.getR0() - 1) % MAX_VALUE;
      curProgram.setR0(value);
      break;
    case I_DEC_R1:
      value = (curProgram.getR1() - 1) % MAX_VALUE;
      curProgram.setR1(value);
      break;
    case I_INC_R0:
      value = (curProgram.getR0() + 1) % MAX_VALUE;
      curProgram.setR0(value);
      break;
    case I_INC_R1:
      value = (curProgram.getR1() + 1) % MAX_VALUE;
      curProgram.setR1(value);
      break;
    case I_JMP_NOT_NULL:
      doubleInc = true;
      if (curProgram.getR0()) {
        value = curProgram.getCode(nextIp);
        jump = true;
        curProgram.setIp(value);
      }
      break;
    case I_JMP_NULL:
      doubleInc = true;
      if (!curProgram.getR0()) {
        value = curProgram.getCode(nextIp);
        jump = true;
        curProgram.setIp(value);
      }
      break;
    case I_MOV_R0_TO_ADD:
      doubleInc = true;
      value = curProgram.getCode(nextIp);
      curProgram.setCode(value, curProgram.getR0());
      break;
    case I_MOV_R1_TO_ADD:
      doubleInc = true;
      value = curProgram.getCode(nextIp);
      curProgram.setCode(value, curProgram.getR1());
      break;
    case I_PRINT:
      curStats.output += curProgram.getR0() > 9 ? 2 : 1;
      break;
    case I_SUB_R1_TO_R0:
      value = (curProgram.getR0() - curProgram.getR1()) % MAX_VALUE;
      curProgram.setR0(value);
      break;
    case I_SUM_R1_TO_R0:
      value = (curProgram.getR0() + curProgram.getR1()) % MAX_VALUE;
      curProgram.setR0(value);
      break;
    case I_SWP_R0_ADD:
      doubleInc = true;
      value = curProgram.getCode(nextIp);
      swap = curProgram.getR0();
      curProgram.setR0(curProgram.getCode(value));
      curProgram.setCode(value, swap);
      break;
    case I_SWP_R1_ADD:
      doubleInc = true;
      value = curProgram.getCode(nextIp);
      swap = curProgram.getR1();
      curProgram.setR1(curProgram.getCode(value));
      curProgram.setCode(value, swap);
      break;

    default:
      ASSERT(false, "Unknown instruction : " << iInst);
  }

  if (!jump) {
    nextIp = doubleInc ? (nextIp+1) % MAX_VALUE : nextIp;
    curProgram.setIp(nextIp);
  }
}

std::string StepForward::perfInformation() const {
  std::ostringstream os;
  os << "StepForwardPerf (total_it=" << totalIterations << ")";
  return os.str();
}

std::ostream& operator <<(std::ostream& oOs, const StepForward& iItem) {
  oOs << "StepForward (" << iItem.curProgram << " / " << iItem.curStats << ")";
  return oOs;
}
