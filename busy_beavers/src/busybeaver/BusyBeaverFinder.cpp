/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#include "BusyBeaverFinder.h"

BusyBeaverFinder::BusyBeaverFinder() {
  const Config& conf = *Config::instance();
  topPrograms.maxSizeHint(conf.topProgramCount);
  undeterminedCount = 0;
  countToPrintProgress = 0;
}
const BusiestBeavers& BusyBeaverFinder::getResults() {
  return topPrograms;
}

// TODO optim : multithreading, 1 thread to generate programs and several program runner threadsconcurrently accessing the top program list
void BusyBeaverFinder::determineTopPrograms() {
  LOG_DEBUG("Chosen generator is : " << generator);
  ExeStats endStats;

  while (generator.hasNext()) {
    // TODO optim : try changing signature to nextProgram(const ProgramState&), it will minimize opject creation
    ProgramState prog = generator.nextProgram();
    runner.checkIfItHalts(prog, endStats);
    topPrograms.pushCopy(prog, endStats);

    INC_PERF_COUNTER_BY(undeterminedCount, endStats.reachedMaxIt);
    countToPrintProgress++;
    progressIndicator();
  }

  if (undeterminedCount)
    LOG_WARN("For some programs the halting question could not be solved : " << undeterminedCount);
  //LOG_WARN("Busiest beaver is : " << topPrograms.busiest().first << " / " << topPrograms.busiest().second);
  LOG_WARN("Busiest beavers found : " << topPrograms);
}

void BusyBeaverFinder::progressIndicator() {
  if (countToPrintProgress & POST_PROGRESS_EVERY) {
    countToPrintProgress = 0;
#ifdef __PERF_STATS__
    LOG_WARN(perfInformation());
#endif
    LOG_WARN("Top so far : " << topPrograms << std::endl);
  }
}

std::string BusyBeaverFinder::perfInformation() const {
  std::ostringstream os;
  os << "BusyBeaverFinderPerf (undetermined=" << undeterminedCount << std::endl
      << "  " << generator.perfInformation() << std::endl
      << "  " << runner.perfInformation() << std::endl
      << ") // BusyBeaverFinderPerf";
  return os.str();
}

std::ostream& operator <<(std::ostream& oOs, const BusyBeaverFinder& iItem) {
  oOs << "BusyBeaverFinder (undetermined=" << iItem.undeterminedCount << ")";
  return oOs;
}
