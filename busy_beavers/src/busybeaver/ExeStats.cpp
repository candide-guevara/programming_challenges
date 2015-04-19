/*
 * Created on: Nov 24, 2013
 *      Author: eguevara
 */

#include <ExeStats.h>

ExeStats::ExeStats(const BigInteger iIterations, const BigInteger iOutput, const bool iStops, const bool iReachedMaxIt)
  : iterations(iIterations), output(iOutput), stops(iStops), reachedMaxIt(iReachedMaxIt) {}

void ExeStats::reset() {
  iterations = 0;
  output = 0;
  reachedMaxIt = stops = false;
}

std::ostream& operator << (std::ostream& oOs, const ExeStats& iExe) {
  oOs << "ExeStats (output=" << iExe.output;
  oOs << ", iterations=" << iExe.iterations;
  oOs << ", stops=" << iExe.stops;
  oOs << ", reachedMaxIt=" << iExe.reachedMaxIt << ")";
  return oOs;
}
