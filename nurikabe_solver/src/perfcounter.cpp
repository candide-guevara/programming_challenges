#include <sstream>
#include <iomanip>
#include <unistd.h>
#include <sys/resource.h>

#include <common.hpp>

namespace EGM {

typedef struct rusage rusage_t;
const double CLOCK_PER_MILLI = sysconf(_SC_CLK_TCK) / 1000.0;
const clock_t UNDEF_TIMER = -1;
const uint64_t STAT_PRECISION = 2;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Private functions
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void updateCounter (PerfRecord& counter, tms_t endCpuTime, clock_t endRealTime) {
  counter.count++;
  counter.totalRealTime += endRealTime - counter.startRealTime;
  counter.totalCpuTime.tms_utime += endCpuTime.tms_utime - counter.startCpuTime.tms_utime;
  counter.totalCpuTime.tms_stime += endCpuTime.tms_stime - counter.startCpuTime.tms_stime;
  counter.totalCpuTime.tms_cutime += endCpuTime.tms_cutime - counter.startCpuTime.tms_cutime;
  counter.totalCpuTime.tms_cstime += endCpuTime.tms_cstime - counter.startCpuTime.tms_cstime;
}

ostream& operator << (ostream& os, const rusage_t& rusage) {
  os << scientific << setprecision(STAT_PRECISION);
  os << "CPU time (millisecs) : user=" << rusage.ru_utime.tv_sec * 1000.0 + rusage.ru_utime.tv_usec / 1000.0
      << " / system=" << rusage.ru_stime.tv_sec * 1000.0 + rusage.ru_stime.tv_usec / 1000.0 << endl
      << "Max resident set size (bytes) : " << rusage.ru_maxrss * 1024.0 << endl
      << "Page faults : minor=" << rusage.ru_minflt * 1.0
      << " / major=" << rusage.ru_majflt * 1.0 << endl
      << "Blocked by I/Os : in=" << rusage.ru_inblock * 1.0
      << " / out=" << rusage.ru_oublock * 1.0 << endl
      << "Context switches : voluntary=" << rusage.ru_nvcsw * 1.0
      << " / involuntary=" << rusage.ru_nivcsw * 1.0 << endl;
  os.copyfmt(RESET_STREAM);
  return os;
}

string tmsToString (const tms_t& tms, const uint64_t count) {
  ostringstream os;
  os << scientific << setprecision(STAT_PRECISION);
  os << "[ total_cpu=" << (tms.tms_utime + tms.tms_stime) / CLOCK_PER_MILLI
      << " / avg_cpu_user=" << tms.tms_utime / (CLOCK_PER_MILLI * count)
      << " / avg_cpu_sys=" << tms.tms_stime / (CLOCK_PER_MILLI * count)
      << " ]";
  return os.str();
}

ostream& operator << (ostream& os, const PerfRecord& record) {
  if (record.count) {
    os << scientific << setprecision(STAT_PRECISION);
    os << "( count=" << record.count * 1.0
        << " / total_real=" << record.totalRealTime / CLOCK_PER_MILLI
        << " / cpu=" << tmsToString(record.totalCpuTime, record.count)
        << " )";
    os.copyfmt(RESET_STREAM);
  }
  else 
    os << "INVALID !!";
  return os;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Method definition
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

PerfRecord::PerfRecord() 
  : count(0), totalCpuTime({0,0,0,0}), startRealTime(UNDEF_TIMER), totalRealTime(0) {}

void PerfCounter::startCounter (string counterKey) {
  _counters[counterKey].startRealTime = times(& (_counters[counterKey].startCpuTime) );
}

void PerfCounter::countOnly (string counterKey) {
  _counters[counterKey].count++;
}

void PerfCounter::stopCounter (string counterKey) {
  tms_t endCpuTime;
  clock_t endRealTime = times(&endCpuTime);
  PerfRecord& counter = _counters[counterKey];

  // We make sure both start and stop counters returned valid times
  if (endRealTime != UNDEF_TIMER && counter.startRealTime != UNDEF_TIMER) 
    updateCounter(counter, endCpuTime, endRealTime);
  else 
    ASSERT(0, "Something went wrong when invoking times");
  counter.startRealTime = UNDEF_TIMER;
}

PerfCounter::~PerfCounter () {
  ostringstream os;
  rusage_t rusage;

  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wunused-variable"
  uint64_t ko = getrusage(RUSAGE_SELF, &rusage);
  ASSERT (!ko, "Could not collect program statistics");
  #pragma GCC diagnostic pop

  os << "Perf statistics : " << endl
      << "Global resource usage : " << endl
      << rusage << endl << endl
      << "Counter statistics (milliseconds) : " << endl;

  for (auto const& counter_pair : _counters)
    os << " + " << counter_pair.first << " = " << counter_pair.second << endl;

  LOG_MONIT(os.str());
}

} // namespace EGM

