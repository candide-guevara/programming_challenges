/*
 * Created on: Nov 30, 2013
 *      Author: eguevara
 */

#ifndef UNITTEST_H_
#define UNITTEST_H_

#include <list>
#include <logging.h>

namespace TST {
  class TestSuite;
}
std::ostream& operator <<(std::ostream& oOs, const TST::TestSuite& iItem);

namespace TST {

typedef std::pair<std::string, void (*) (void)> TestMethod;
typedef std::pair<bool, std::string> TestResult;

class TestSuite {
  public:
    TestSuite() {}
    static TestSuite& instance() {
      static TestSuite singleton;
      return singleton;
    }

    void addTest(const std::string& iName, void (*testMethod) (void));
    void addResult(const bool iSuccess, const std::string& iMes);

    void runAllTests();
    void runSingleTest(const TestMethod& iTest);

    std::string printResults() const;

    friend std::ostream& ::operator <<(std::ostream& oOs, const TestSuite& iItem);

  private:
    std::list<TestMethod> tests;
    std::list<TestResult> results;
    bool testOutcomeDecided;
};

#define TST_ASSERT(exp, mes) \
  if(!(exp)) { \
    std::ostringstream os; \
    os << mes; \
    TestSuite::instance().addResult(false, os.str()); \
    return; \
  }

#define TST_REGISTER(name) (TestSuite::instance().addTest(#name, name))

}

#endif /* UNITTEST_H_ */
