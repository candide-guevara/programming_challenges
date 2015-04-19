#ifndef UNITTEST_H_
#define UNITTEST_H_

#include <map>
#include <exception>
#include <common.hpp>

#define TEST_ASSERT(exp,msg) \
  if (!(exp)) { LOG_ASSERT(exp,msg); throw EGM::TestFailedException(#msg); } 

#define UNIT_TEST_CLASS(name,parent)                    \
  class name : public EGM::parent {                     \
    public:                                             \
      name() {                                          \
        EGM::BaseUnitTest::allTests()[#name] = this;    \
        _name = #name;                                  \
      }                                                 \
      void test();                                      \
  };

#define UNIT_TEST(name,parent)    \
  UNIT_TEST_CLASS(name,parent)    \
  static name instance_##name;    \
  void name::test ()                                     

#define SIMPLE_TEST(name) UNIT_TEST(name,BaseUnitTest)

namespace EGM {

/**
 * Throw this in unit tests if some check fails. The test assertion macros should use this one too.
 */
class TestFailedException : public AssertFailedException {
  public:
    TestFailedException(const string& msg) : AssertFailedException(msg) {}
};

/**
 * Base class for all unit tests. Uses the virtual constructor pattern to gather all tests at compile time.
 */
class BaseUnitTest {
  public: 
    /// Put common code here (stats, logs ...)
    void testWrapper ();
    /// To be implemented by each unit test
    virtual void test () = 0;

    string _name;
    static map<string, BaseUnitTest*>& allTests();
};

/**
 * Runs the unit tests and outputs a report.
 */
class TestRunner {
  public:
    TestRunner();
    void runAllTests();
    void runAllTestsMatching(string iPattern);
    void runSingleTest(string testName);

  private:
    void runSingleTest(ostream& os, string testName);
    void outputReport(ostream& os);

    uint64_t _total, _run_ok, _run_fail, _run_error;
};

} // namespace EGM

#endif /* UNITTEST_H_ */
