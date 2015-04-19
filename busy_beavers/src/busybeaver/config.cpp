/*
 * Created on: Nov 29, 2013
 *      Author: eguevara
 */

#include <config.h>
#include <unistd.h>

const char* USAGE = "\n\n"
"USAGE: busy_beaver_finder [OPTIONS]\n"
"  -h    Displays this message\n"
"  -t    Runs the test suite\n"
"  -i    Number of execution cycles before giving up and consider the program will not halt\n"
"  -m    Max number of programs (and halt or not) to keep in memory [power of 2]\n"
"  -r    Max number of programs to generate and check if they halt [power of 2]\n"
"  -l/x  Min/max bytes in the generated candidate programs [from 1 to 16]\n"
"  -c    For each program execution remember at most -c previous states as halting or not\n"
"  -d    Remember program state every -d execution cycles\n"
"  -e    Do not generate candidate programs containing these instructions [bitset]\n"
"  -f    Discards some program candidates before running them. Needed by -e [0 or 1]\n";

void Config::setDefaults() {
  maxProgramIteration = 100000;
  maxInMemoryPrograms = 1 << 24;
  saveCountPerRun = 50;
  saveDropRatio = 1000;
  maxProgramPropositions = 1 << 24;
  minProgramLen = 10;
  maxProgramLen = 16;
  topProgramCount = 5;
  activateCandidateFiltering = true;
  runTestSuite = false;
  forbiddenInstructions = 0;
}

void Config::setDefaultForTest() {
  maxProgramIteration = 1000;
  maxInMemoryPrograms = 1 << 20;
  saveCountPerRun = 50;
  saveDropRatio = 100;
  maxProgramPropositions = 1 << 10;
  minProgramLen = 2;
  maxProgramLen = 4;
  topProgramCount = 5;
  activateCandidateFiltering = false;
  runTestSuite = true;
  forbiddenInstructions = 0;
}

uint32_t Config::parseBitSet (const char *iStr) const {
  uint32_t bitset = 0;
  for (uint32_t idx = 0; iStr[idx]; ++idx)
    bitset += iStr[idx] == '1' ? (1 << idx) : 0;
  return bitset;
}

void Config::loadOptions(int argc, char **argv) {
  setDefaults();
  int c;
  while ((c = getopt(argc, argv, "i:m:r:l:x:c:d:e:f:ht")) != -1)
    switch (c) {
      case 'i':
        maxProgramIteration = std::atoi(optarg);
        break;
      case 'm':
        maxInMemoryPrograms = 1 << std::atoi(optarg);
        break;
      case 'r':
        maxProgramPropositions = 1 << std::atoi(optarg);
        break;
      case 'l':
        minProgramLen = std::atoi(optarg);
        break;
      case 'x':
        maxProgramLen = std::atoi(optarg);
        break;
      case 'c':
        saveCountPerRun = std::atoi(optarg);
        break;
      case 'd':
        saveDropRatio = std::atoi(optarg);
        break;
      case 'e':
        forbiddenInstructions = parseBitSet(optarg);
        break;
      case 'f':
        activateCandidateFiltering = std::atoi(optarg);
        break;
      case 't':
        runTestSuite = true;
        break;
      case '?':
      default:
        LOG_INFO(USAGE);
        exit(1);
    }
    if (optind != argc) {
      LOG_INFO(USAGE);
      exit(1);
    }
}

std::ostream& operator <<(std::ostream& oOs, const Config& iItem) {
  oOs << "Configuration (" << "maxProgramIteration=" << iItem.maxProgramIteration
      << ", maxInMemoryPrograms=" << iItem.maxInMemoryPrograms
      << ", saveCountPerRun=" << iItem.saveCountPerRun << std::endl
      << ", saveDropRatio=" << iItem.saveDropRatio
      << ", maxProgramPropositions=" << iItem.maxProgramPropositions
      << ", minProgramLen=" << iItem.minProgramLen << std::endl
      << ", maxProgramLen=" << iItem.maxProgramLen
      << ", topProgramCount=" << iItem.topProgramCount
      << ", activateCandidateFiltering=" << iItem.activateCandidateFiltering << std::endl
      << ", runTestSuite=" << iItem.runTestSuite << ")";
  return oOs;
}
