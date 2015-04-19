/*
 * Created on: Dec 1, 2013
 *      Author: eguevara
 */

#include <unittest.h>
#include <BusyBeaverFinder.h>

namespace TST {

  // For this test to work it is best to have the complete generator enabled
  void TestSmallLenPrograms() {
    Config& conf = *Config::instance();
    conf.minProgramLen = 1;
    conf.maxProgramLen = 2;
    // For such small programs filtering will produce incorrect results
    conf.activateCandidateFiltering = false;

    BusyBeaverFinder finder;
    finder.determineTopPrograms();
    const auto& busy = finder.getResults().busiest();
    auto busySecond = ----finder.getResults().end();

    conf.minProgramLen = 0;
    BusyBeaverFinder finder2;
    finder2.determineTopPrograms();
    const auto& busy2 = finder2.getResults().busiest();
    auto busySecond2 = ----finder2.getResults().end();

    TST_ASSERT(busy.second.output == 2 && busy2.second.output == 2,
        "Could not determine busiest beaver for len " << conf.maxProgramLen);
    TST_ASSERT(busySecond->second.output == 2 && busySecond2->second.output == 2, "Could not determine second busiest beaver");
  }

  void testForProgramLen(unsigned len, BigInteger output);
  void TestLessSmallLenPrograms() {
    //testForProgramLen(3, 22);
    testForProgramLen(4, 44);
  }

  void testForProgramLen(unsigned len, BigInteger output) {
    Config& conf = *Config::instance();
    conf.minProgramLen = len-1;
    conf.maxProgramLen = len;
    conf.activateCandidateFiltering = false;
    conf.maxProgramPropositions = 1 << 18;

    BusyBeaverFinder finder;
    finder.determineTopPrograms();
    const auto& busy = finder.getResults().busiest();

    conf.minProgramLen = 0;
    BusyBeaverFinder finder2;
    finder2.determineTopPrograms();
    const auto& busy2 = finder2.getResults().busiest();

    conf.activateCandidateFiltering = true;
    BusyBeaverFinder finder3;
    finder3.determineTopPrograms();
    const auto& busy3 = finder3.getResults().busiest();

    conf.minProgramLen = len-2;
    BusyBeaverFinder finder4;
    finder4.determineTopPrograms();
    const auto& busy4 = finder4.getResults().busiest();

    LOG_TEST("1. " << finder.getResults());
    LOG_TEST("2. " << finder2.getResults());
    LOG_TEST("3. " << finder3.getResults());
    LOG_TEST("4. " << finder4.getResults());

    TST_ASSERT(
        busy.second.output == output && busy2.second.output == output && busy3.second.output == output && busy4.second.output == output,
        "Could not determine busiest beaver for len " << conf.maxProgramLen);

    // Not valid for random program generators
    // TST_ASSERT(busy.first.slowCompare(busy2.first), "Discrepancies between results 1 and 2");
    // TST_ASSERT(busy.first.slowCompare(busy3.first), "Discrepancies between results 1 and 3");
    // TST_ASSERT(busy.first.slowCompare(busy4.first), "Discrepancies between results 1 and 4");
  }
}  // namespace TST
