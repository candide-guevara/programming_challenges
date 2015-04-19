/*
 * Created on: Nov 30, 2013
 *      Author: eguevara
 */

#include <unittest.h>
#include <ProgramState.h>

namespace TST {

  void testOperationsOnState () {
    ProgramState prog;
    uint8_t testCode[20] = { I_INC_R0, I_PRINT, I_INC_R1, I_MOV_R0_TO_ADD, I_INC_R1, I_INC_R0, I_INC_R0, I_INC_R0, I_HALT };

    prog.loadCode(testCode);
    TST_ASSERT(prog.getCode(0) == I_INC_R0 && prog.getCode(4) == I_INC_R1 && prog.getCode(8) == I_HALT, "Failed to load code");

    prog.setIp(8);
    TST_ASSERT(prog.hasStopped(), "Failed to detect halting program");

    prog.setR1(4);
    prog.resetRegisters();
    TST_ASSERT(!prog.getR0() && !prog.getR1() && !prog.getIp(), "Bad reset register state");

    bool serializeOk = true;
    for (unsigned i = 0; i < MAX_CODE_LEN; ++i)
      if (prog.getSerializedState()[i] != testCode[i])
        serializeOk = false;
    TST_ASSERT(serializeOk, "Program state serialization failure");
  }

}  // namespace TST


