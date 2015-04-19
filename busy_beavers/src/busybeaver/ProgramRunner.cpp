/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#include "ProgramRunner.h"
#include <config.h>

ProgramRunner::ProgramRunner() {
  const Config& conf = *Config::instance();

  previousStates.maxSizeHint(conf.maxProgramIteration / 16);
  pastPrograms.maxSizeHint(conf.maxInMemoryPrograms * 6);
  statesToSave.setSaveUpto(conf.saveCountPerRun);
  statesToSave.setSaveDropRatio(conf.saveDropRatio);
  tryUpToIterations = conf.maxProgramIteration;
  memLimit = conf.maxInMemoryPrograms;

  cacheHitCount = 0;
  previousStateHitCount = 0;
  longestRunning = 0;
}

void ProgramRunner::dropHalfOfStoredStates() {
  LOG_WARN("ProgramRunner::dropHalfOfStoredStates " << pastPrograms.size());
  pastPrograms.clear();
}

void ProgramRunner::checkIfItHalts(const ProgramState& iInitState, ExeStats& oEndStats) {
  LOG_DEBUG("Checking if program halts : " << iInitState);
  previousStates.clear();
  statesToSave.clear();
  ExeStats* alreadySeen = pastPrograms.get(iInitState);

  if (alreadySeen) {
    oEndStats = *alreadySeen;
    INC_PERF_COUNTER(cacheHitCount);
  }
  else
    runAProgramWeHaveNotSeenBefore(iInitState, oEndStats);

  TAKE_MAX_PERF(longestRunning, oEndStats.iterations);
  LOG_INFO("Run done, init state : " << iInitState << std::endl
      << "  final state : " << emulator.getCurProgram() << std::endl
      << "  stats : " << oEndStats);
}

void ProgramRunner::runAProgramWeHaveNotSeenBefore(const ProgramState& iInitState, ExeStats& oEndStats) {
  if (pastPrograms.size() > memLimit)
    dropHalfOfStoredStates();

  // This is a new program we will need to run it to see if it halts
  emulator.load(iInitState, tryUpToIterations);
  const ExeStats& curStats = emulator.getCurStats();

  runUntilHaltsOrMaxIt();
  ASSERT(curStats.iterations && (!curStats.stops || !curStats.reachedMaxIt),
      "Calculated an incorrect current state " << curStats);

  statesToSave.saveIntoMap(pastPrograms, curStats);
  oEndStats = curStats;
}

void ProgramRunner::runUntilHaltsOrMaxIt() {
  // Direct access to emulator inner state, not great ...
  ExeStats& curStats = emulator.modifyCurStats();

  while (true) {
    emulator.advanceOnce();

    if (emulator.getCurStats().stops || emulator.getCurStats().reachedMaxIt) {
      LOG_DEBUG("The program has halted or we give up going further : " << emulator.getCurProgram());
      curStats.reachedMaxIt = !curStats.stops;
      return;
    }

    if (previousStates.contains(emulator.getCurProgram())) {
      LOG_DEBUG("We have already reached this state during the execution => this program loops : " << emulator.getCurProgram());
      INC_PERF_COUNTER(previousStateHitCount);
      curStats.reachedMaxIt = false;
      curStats.stops = false;
      statesToSave.absolutelySaveForLater(emulator.getCurProgram(), emulator.getCurStats());
      return;
    }

    ExeStats* alreadySeen = pastPrograms.get(emulator.getCurProgram());
    if (alreadySeen) {
      LOG_DEBUG("We have reached this program state before, no need to go further : " << emulator.getCurProgram());
      LOG_DEBUG("The final execution stats should be : " << *alreadySeen);
      INC_PERF_COUNTER(cacheHitCount);

      if (alreadySeen->stops) {
        // Case1 - the program will eventually stop and we know the FINAL iteration and output count
        // FROM this state
        curStats.iterations += alreadySeen->iterations;
        curStats.output += alreadySeen->output;
        curStats.reachedMaxIt = false;
        curStats.stops = true;
      }
      else {
        // Case2 - we GUESS the program will never stop, the iteration and output count are not relevant
        curStats.reachedMaxIt = alreadySeen->reachedMaxIt;
        curStats.stops = false;
      }
      return;
    }

    statesToSave.maybeSaveForLater(emulator.getCurProgram(), emulator.getCurStats());
    previousStates.addCopy(emulator.getCurProgram());
  } // while true
}

std::string ProgramRunner::perfInformation() const {
  std::ostringstream os;
  os << "ProgramRunnerPerf : longest=" << longestRunning << std::endl
      << "  " << emulator.perfInformation() << std::endl
      << "  Past_hit_count=" << cacheHitCount << " / " << pastPrograms.perfInformation() << std::endl
      << "  Previous_hit_count=" << previousStateHitCount << " / " << previousStates.perfInformation();
  return os.str();
}

std::ostream& operator <<(std::ostream& oOs, const ProgramRunner& iItem) {
  oOs << "ProgramRunner (maxIt=" << iItem.tryUpToIterations << std::endl
      << "  previous_states=" << iItem.previousStates << std::endl
      << "  past_states=" << iItem.pastPrograms << std::endl
      << "  states_to_save=" << iItem.statesToSave << std::endl
      << ") // ProgramRunner";
  return oOs;
}
