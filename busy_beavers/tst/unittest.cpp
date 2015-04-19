/*
 * Created on: Nov 30, 2013
 *      Author: eguevara
 */

#include <unittest.h>
#include <alltest.h>
#include <config.h>

namespace TST {

  void TestSuite::addTest(const std::string& name, void (*testMethod)(void)) {
    TestMethod test(name, testMethod);
    tests.push_back(test);
  }

  void TestSuite::addResult(const bool iSuccess, const std::string& iMes) {
    if (!testOutcomeDecided) {
      TestResult result(iSuccess, iMes);
      results.push_back(result);
    }
    testOutcomeDecided = true;
  }

  void TestSuite::runAllTests() {
    registerAll();
    ::Config& conf = *::Config::instance();
    conf.setDefaultForTest();
    LOG_DEBUG("Default config for tests : " << conf);

    LOG_TEST("Starting run for " << tests.size() << " tests");
    for (auto it = tests.begin(); it != tests.end(); ++it)
      runSingleTest(*it);

    ASSERT(tests.size() == results.size(), "Number of tests != to results");
    LOG_TEST("Finished run");
  }

  void TestSuite::runSingleTest(const TestMethod& iTest) {
    LOG_TEST("Running : " << iTest.first);
    ::Config& conf = *::Config::instance();
    conf.setDefaultForTest();
    testOutcomeDecided = false;
    try {
      iTest.second();
      addResult(true, "Success");
    }
    catch (std::exception& error) {
      addResult(false, std::string() + "Test failed with exception " + error.what());
    }
    catch (...) {
      addResult(false, "Test failed with exception");
    }
    LOG_DEBUG("Finished with : " << iTest.first);
  }

  std::string TestSuite::printResults() const {
    std::ostringstream aOs;
    unsigned totalFail=0, totalOk=0;
    auto tt = tests.begin();
    auto rt = results.begin();
    aOs << std::endl;

    for (; tt != tests.end(); ++tt, ++rt) {
      totalFail += rt->first ? 0 : 1;
      totalOk += rt->first ? 1 : 0;
      aOs << " + Test : " << tt->first << " => " << rt->first << " (" << rt->second << ")" << std::endl;
    }
    aOs << "Results (success=" << totalOk << ", fail=" << totalFail << ")";
    return aOs.str();
  }
}

std::ostream& operator <<(std::ostream& oOs, const TST::TestSuite& iItem) {
  oOs << "TestSuite (tests=" << iItem.tests.size() << ", results=" << iItem.results.size() << ")";
  return oOs;
}
