#include <sstream>
#include <iomanip>

#include <boost/regex.hpp>
#include <unittest.hpp>

namespace EGM {
  
map<string, BaseUnitTest*>& BaseUnitTest::allTests () {
  static map<string, BaseUnitTest*>* tests = new map<string, BaseUnitTest*>;
  return *tests;
}

void BaseUnitTest::testWrapper () {
  // Does nothing by default
  LOG_INFO("--------------------------------- Running : " << _name << " ---------------------------------------------");
  this->test();
}

TestRunner::TestRunner()
  : _total(0), _run_ok(0), _run_fail(0), _run_error(0) {}

void TestRunner::runSingleTest (ostream& os, string testName) {
  auto test = BaseUnitTest::allTests().find(testName);
  if (test == BaseUnitTest::allTests().end() || !(test->second) ) {
    LOG_WARN("Test " << testName << " is not defined !");
    return;
  }
  
  try {
    _total++;
    os << " + Running " << testName << " : ";
    test->second->testWrapper();
    _run_ok++;
    os << "OK" << endl;
  }
  catch (TestFailedException& err) {
    _run_fail++;
    os << "Failed - " << err.what() << endl;
  }
  catch (exception& err) {
    _run_error++;
    os << "Error - " << err.what() << endl;
  }
  catch (...) {
    _run_error++;
    os << "Error  - unknown !" << endl;
  }
}

void TestRunner::runSingleTest (string testName) {
  ostringstream os;
  runSingleTest(os, testName);
  LOG_INFO(os.str());
}

void TestRunner::outputReport (ostream& os) {
  os << setprecision(1) << fixed;
  if (_total) {
    os << "Total run : " << _total << endl
        << " - OK     :  " << _run_ok << " (" << _run_ok * 100.0 / _total << "%)" << endl
        << " - Failed :  " << _run_fail << " (" << _run_fail * 100.0 / _total << "%)" << endl
        << " - Error  :  " << _run_error << " (" << _run_error * 100.0 / _total << "%)" << endl;
    if (_total == _run_ok)
      os << COL_BOLDGREEN << "All tests passed :-D" << COL_RESET << endl;
    else
      os << COL_BOLDYELLOW << "Some tests failed :-(" << COL_RESET << endl;
  }
  else
    os << COL_BOLDYELLOW << "No tests run :-(" << endl << COL_RESET;
  os.copyfmt(RESET_STREAM);    
}

void TestRunner::runAllTests () {
  auto const& allTests = BaseUnitTest::allTests();
  ostringstream os;

  os << "Launching test suite with " << allTests.size() << " items" << endl;
  for (auto const& test_pair : allTests)
    runSingleTest(os, test_pair.first);

  outputReport(os);
  LOG_MONIT(os.str());
}

void TestRunner::runAllTestsMatching (string iPattern) {
  boost::regex testRx(iPattern, boost::regex_constants::icase); 
  auto const& allTests = BaseUnitTest::allTests();
  ostringstream os;

  os << "Launching tests matching " << iPattern << endl;
  for (auto const& test_pair : allTests)
    if ( boost::regex_search(test_pair.first, testRx) )
      runSingleTest(os, test_pair.first);

  outputReport(os);
  LOG_MONIT(os.str());
}

} // namespace EGM

/// Simply run the all the tests when launching the program.
int main (int argc, char** argv) {
  STAT_HIGH_PRIO_START(TestRunner::runTests);

  if (argc < 2)
    EGM::TestRunner().runAllTests();
  else 
    EGM::TestRunner().runAllTestsMatching(argv[1]);

  STAT_HIGH_PRIO_STOP(TestRunner::runTests);
  return 0;
}

