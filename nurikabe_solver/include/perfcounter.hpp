#ifndef PREFCOUNTER_H_
#define PREFCOUNTER_H_

#include <cstdint>
#include <sys/times.h>
#include <map>

#include <logger.hpp>

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// USE ONLY THIS MACROS !!
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifndef __PERF_STATS__
  #define __PERF_STATS__ 2
#endif

#if __PERF_STATS__ > 1
  #define STAT_START(name)  EGM::PerfCounter::instance().startCounter(#name)
  #define STAT_STOP(name)   EGM::PerfCounter::instance().stopCounter(#name)
  #define STAT_COUNT(name)  EGM::PerfCounter::instance().countOnly(#name)
#else
  #define STAT_START(name) 
  #define STAT_STOP(name) 
  #define STAT_COUNT(name)
#endif

#if __PERF_STATS__ > 0
  #define STAT_HIGH_PRIO_START(name)  EGM::PerfCounter::instance().startCounter(#name)
  #define STAT_HIGH_PRIO_STOP(name)   EGM::PerfCounter::instance().stopCounter(#name)
#else
  #define STAT_HIGH_PRIO_START(name) 
  #define STAT_HIGH_PRIO_STOP(name) 
#endif

namespace EGM {

typedef struct tms tms_t;

/**
 * Holds the information for a single perf record.
 */
struct PerfRecord {
  PerfRecord();

  uint64_t count;
  tms_t startCpuTime;
  tms_t totalCpuTime;
  clock_t startRealTime;
  clock_t totalRealTime;
};

/**
 * Contains all the perf counters. It will dump them into the logs at the end of the program.
 */
class PerfCounter {
  public:
    /// Gets the singleton instance
    static PerfCounter& instance() { static PerfCounter counter; return counter; }
    
    /// Dumps all perf counters at destruction. Since we use a singleton it will be done at program termination.
    ~PerfCounter();

    /// only counts how many times the counter has been called no time info but lower overhead
    void countOnly (std::string counterKey);

    /// Start one run of the named counter
    void startCounter(std::string counterKey);

    /// Stops the previously started run of the named counter
    void stopCounter(std::string counterKey);

  private:  
    std::map<std::string, PerfRecord> _counters;
};

} // namespace EGM
#endif /* PREFCOUNTER_H_ */

