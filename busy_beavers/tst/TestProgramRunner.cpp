/*
 * Created on: Nov 30, 2013
 *      Author: eguevara
 */

#include <unittest.h>
#include <ProgramRunner.h>

namespace TST {

  void TestProgramEmulator() {
    ProgramState prog;
    StepForward emulator;
    uint8_t testCode[20] = { I_INC_R1, I_PRINT, I_INC_R1, I_ABS_TO_R0, 12, I_PRINT, I_DEC_R1, I_INC_R1, I_HALT };
    uint8_t testCode2[20] = { I_JMP_NULL, 3, I_HALT, I_DEC_R0, I_PRINT, I_SWP_R1_ADD, 1, I_SUB_R1_TO_R0, I_PRINT, I_HALT };

    prog.loadCode(testCode);
    prog.resetRegisters();
    emulator.load(prog, 100);
    emulator.advanceOnce();
    TST_ASSERT(emulator.getCurStats().iterations == 1 && emulator.getCurStats().output == 0 && emulator.getCurProgram().getR1() == 1,
        "Failed to execute an instrunction");

    emulator.advanceOnce();
    TST_ASSERT(emulator.getCurStats().iterations == 2 && emulator.getCurStats().output == 1 && emulator.getCurProgram().getR1() == 1,
        "Failed to execute an instrunction (2)");

    for (unsigned i = 0; i < 5; ++i)
      emulator.advanceOnce();
    TST_ASSERT(emulator.getCurProgram().hasStopped(), "Failed to emulate program halt");
    TST_ASSERT(emulator.getCurStats().iterations == 7 && emulator.getCurStats().output == 3 && emulator.getCurProgram().getR1() == 2,
        "Failed to execute several instrunctions");

    prog.loadCode(testCode2);
    prog.resetRegisters();
    emulator.load(prog, 100);
    for (unsigned i = 0; i < 6; ++i)
      emulator.advanceOnce();
    TST_ASSERT(emulator.getCurProgram().hasStopped(), "Failed to emulate program halt (2)");
    TST_ASSERT(
        emulator.getCurStats().output == 4 && emulator.getCurProgram().getR0() == 12 && emulator.getCurProgram().getCode(1) == 0,
        "Failed to execute several instrunctions " << emulator);
  }

  void TestHaltingPrograms() {
    Config& conf = *Config::instance();
    conf.saveDropRatio = 5;
    ProgramState prog;
    ProgramRunner runner;
    ExeStats stat, stat2;
    uint8_t testCode[20] = {
        I_JMP_NULL, 3, I_HALT, I_DEC_R0, I_PRINT, I_SWP_R1_ADD, 1, I_SUB_R1_TO_R0, I_PRINT, I_JMP_NULL, I_HALT };
    uint8_t testCode2[20] = {
        I_PRINT, I_PRINT, I_ABS_TO_R0, I_PRINT, I_MOV_R0_TO_ADD, 0, I_MOV_R0_TO_ADD, 1, I_PRINT, I_DEC_R1, I_DEC_R0, I_HALT };
    uint8_t testCode3[20] = {
        I_JMP_NULL, 2, I_ABS_TO_R0, I_PRINT, I_MOV_R0_TO_ADD, 0, I_MOV_R0_TO_ADD, 1, I_PRINT, I_DEC_R1, I_DEC_R0, I_HALT };

    prog.loadCode(testCode);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(stat.output == 4 && stat.stops, "Failed to execute halting program");
    TST_ASSERT(runner.previousStates.size() == stat.iterations-1, "Did not store all intermediate execution states");

    BigInteger storedCount = runner.pastPrograms.size();
    BigInteger statesBeforeHalting = runner.previousStates.size();

    runner.checkIfItHalts(prog, stat2);
    TST_ASSERT(stat.output == stat2.output && stat.stops == stat2.stops, "Failed to execute halting program (2)");
    TST_ASSERT(runner.previousStates.size() < statesBeforeHalting, "Failed to use previous data to predict program outcome");
    runner.checkIfItHalts(prog, stat2);
    TST_ASSERT(runner.pastPrograms.size() == storedCount, "Stored useless intermediate execution states");

    prog.loadCode(testCode2);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(stat.output == 3 && stat.stops, "Failed to execute halting program (3)");

    storedCount = runner.pastPrograms.size();
    statesBeforeHalting = runner.previousStates.size();

    prog.loadCode(testCode3);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat2);
    TST_ASSERT(runner.previousStates.size() < statesBeforeHalting, "Failed to use previous data to predict program outcome (2)");
  }

  void TestShortLoopingPrograms() {
    ProgramState prog;
    ProgramRunner runner;
    ExeStats stat;
    uint8_t testCode[20] = { I_JMP_NULL, 0, I_DEC_R0, I_PRINT, I_HALT };
    uint8_t testCode2[20] = { I_JMP_NOT_NULL, 0, I_DEC_R0, I_JMP_NOT_NULL, 0, I_HALT };
    uint8_t testCode3[20] =
        { I_PRINT, I_PRINT, I_ABS_TO_R0, I_JMP_NOT_NULL, I_MOV_R0_TO_ADD, 0, I_ABS_TO_R0, 0, I_MOV_R0_TO_ADD, 1, I_JMP_NULL, 0, I_HALT };

    prog.loadCode(testCode);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(!stat.output && !stat.stops && !stat.reachedMaxIt, "Failed to execute looping program");

    BigInteger statesBeforeHalting = runner.previousStates.size();
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(!stat.output && !stat.stops && !stat.reachedMaxIt, "Failed to execute looping program (2)");
    TST_ASSERT(runner.previousStates.size() < statesBeforeHalting, "Failed to use previous data to predict program outcome");

    prog.loadCode(testCode2);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(!stat.stops && !stat.reachedMaxIt, "Failed to execute looping program (3)");

    prog.loadCode(testCode3);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(!stat.stops && !stat.reachedMaxIt, "Failed to execute looping program (4)");
  }

  void TestLongLoopingPrograms() {
    ::Config& conf = *::Config::instance();
    conf.maxProgramIteration = 20;
    ProgramState prog;
    ProgramRunner runner;
    ExeStats stat;
    uint8_t testCode[20] = { I_INC_R0, I_INC_R1, I_JMP_NOT_NULL, 0, I_HALT };
    uint8_t testCode2[20] ={ I_PRINT, I_PRINT, I_ABS_TO_R0, I_JMP_NOT_NULL, I_INC_R1, I_MOV_R0_TO_ADD, 0, I_ABS_TO_R0, 0,
        I_MOV_R0_TO_ADD, 1, I_JMP_NULL, 0, I_HALT };

    prog.loadCode(testCode);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(!stat.stops && stat.reachedMaxIt, "Failed to execute long running looping program");

    prog.loadCode(testCode2);
    prog.resetRegisters();
    runner.checkIfItHalts(prog, stat);
    TST_ASSERT(!stat.stops && stat.reachedMaxIt, "Failed to execute long running looping program (2)");
  }

}  // namespace TST

