/*
 * Created on: Nov 30, 2013
 *      Author: eguevara
 */

#ifndef ALLTEST_H_
#define ALLTEST_H_

#include <unittest.h>

// very painful way to declare unit tests (there is not static init block in c++) ...

namespace TST {

  void testOperationsOnState();

  void testBusiestProgramQueue();
  void TestProgramToSaveCommonOperations();
  void TestProgramToSaveMergingWithMap();
  void TestProgramMapInsertion();

  void TestProgramEmulator();
  void TestHaltingPrograms();
  void TestShortLoopingPrograms();
  void TestLongLoopingPrograms();

  void testCompleteGenerator();
  void testCompleteGeneratorAllProgramSpace();
  void testCompleteGeneratorBorderLineLengths();
  void testRandomGenerator();
  void testRandomGeneratorGenerateAll();
  void testCandidateFiltering();
  void TestFilteringByInstructions();

  void TestSmallLenPrograms ();
  void TestLessSmallLenPrograms ();

  void registerAll() {
    TST_REGISTER(testOperationsOnState);

    TST_REGISTER(testBusiestProgramQueue);
    TST_REGISTER(TestProgramMapInsertion);
    TST_REGISTER(TestProgramToSaveCommonOperations);
    TST_REGISTER(TestProgramToSaveMergingWithMap);

    TST_REGISTER(TestProgramEmulator);
    TST_REGISTER(TestHaltingPrograms);
    TST_REGISTER(TestShortLoopingPrograms);
    TST_REGISTER(TestLongLoopingPrograms);

    TST_REGISTER(testCandidateFiltering);
    TST_REGISTER(TestFilteringByInstructions);
    TST_REGISTER(testRandomGenerator);
    TST_REGISTER(testRandomGeneratorGenerateAll);
    TST_REGISTER(testCompleteGenerator);
    TST_REGISTER(testCompleteGeneratorAllProgramSpace);
    TST_REGISTER(testCompleteGeneratorBorderLineLengths);

    TST_REGISTER(TestSmallLenPrograms);
    TST_REGISTER(TestLessSmallLenPrograms);
  }

}  // namespace TST

#endif /* ALLTEST_H_ */
