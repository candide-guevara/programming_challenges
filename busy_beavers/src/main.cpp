/*
 * Created on: Nov 23, 2013
 *      Author: eguevara
 */

#include <BusyBeaverFinder.h>
#include <unittest.h>

int main(int argc, char **argv) {
  LOG_WARN("Starting busy beaver finder");
  Config* conf = Config::instance();
  conf->loadOptions(argc, argv);
  LOG_DEBUG("Command line arguments : " << *conf);

  if (conf->runTestSuite) {
    TST::TestSuite& suite = TST::TestSuite::instance();
    suite.runAllTests();
    LOG_WARN(suite.printResults());
  }
  else {
    BusyBeaverFinder finder;
    finder.determineTopPrograms();
  }
  LOG_WARN("All done !!!");
}

