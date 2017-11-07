#include <common.h>

#include <sys/resource.h>

#include <assert.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <logger.h>

IGNORE_WARNING_PUSH("-Wunused-function")
// Use this to set breakpoints to trap assertion violations
void __my_assert__(int condition) { assert(condition); }
IGNORE_WARNING_POP

/////////////////////////////////////////////////////////////////////////////////////////////////

#define FIELD_ADD_DIFF(field, dest, start, stop) (dest).field += (stop).field - (start).field

#define TV_ADD_DIFF(dest, start, stop)                                                                                 \
  FIELD_ADD_DIFF(ru_utime.tv_sec, dest, start, stop);                                                                  \
  FIELD_ADD_DIFF(ru_utime.tv_usec, dest, start, stop);                                                                 \
  FIELD_ADD_DIFF(ru_stime.tv_sec, dest, start, stop);                                                                  \
  FIELD_ADD_DIFF(ru_stime.tv_usec, dest, start, stop)

#define TV_TO_SECS(elapsed) (double)elapsed.tv_sec + (double)(elapsed.tv_usec) / 1000000

// static vars are always zero init in c99
static struct rusage __tmp_chrono__[__CHRONO_LAST__];
static struct rusage __total_chrono__[__CHRONO_LAST__];

DEFINE_ENUM_TO_STRING(ChronoId, CHRONOID_ENUM)

void __chrono_start__(ChronoId id) { getrusage(RUSAGE_THREAD, __tmp_chrono__ + id); }

void __chrono_stop__(ChronoId id) {
  struct rusage final_value;
  getrusage(RUSAGE_THREAD, &final_value);

  TV_ADD_DIFF(__total_chrono__[id], __tmp_chrono__[id], final_value);
  FIELD_ADD_DIFF(ru_minflt, __total_chrono__[id], __tmp_chrono__[id], final_value);
  FIELD_ADD_DIFF(ru_majflt, __total_chrono__[id], __tmp_chrono__[id], final_value);
  FIELD_ADD_DIFF(ru_inblock, __total_chrono__[id], __tmp_chrono__[id], final_value);
  FIELD_ADD_DIFF(ru_oublock, __total_chrono__[id], __tmp_chrono__[id], final_value);
}

const struct rusage *get_chrono(ChronoId id) { return __total_chrono__ + id; }

void diff_chrono_by_id(struct rusage *result, ChronoId id_start, ChronoId id_stop) {
  return diff_chrono_by_obj(result, get_chrono(id_start), get_chrono(id_stop));
}

void diff_chrono_by_obj(struct rusage *result, const struct rusage *start, const struct rusage *stop) {
  clear_chrono_by_obj(result);
  TV_ADD_DIFF(*result, *start, *stop);
  FIELD_ADD_DIFF(ru_minflt, *result, *start, *stop);
  FIELD_ADD_DIFF(ru_majflt, *result, *start, *stop);
  FIELD_ADD_DIFF(ru_inblock, *result, *start, *stop);
  FIELD_ADD_DIFF(ru_oublock, *result, *start, *stop);
}

void clear_chrono_by_id(ChronoId id) { clear_chrono_by_obj(__total_chrono__ + id); }

void clear_chrono_by_obj(struct rusage *chrono) { memset(chrono, 0, sizeof(struct rusage)); }

///////////////////////////////////////////////////////////////////////////////////////////

int chrono_header_to_csv(char *buffer, int buflen) {
  return snprintf(buffer, buflen,
                  "name, total_time, user_time, sys_time, major_page_flt, minor_page_flt, io_ops_in, io_ops_out\n");
}

int chrono_to_csv_by_id(char *buffer, int buflen, ChronoId id) {
  return chrono_to_csv_by_obj(buffer, buflen, ChronoId_to_string(id), get_chrono(id));
}

int chrono_to_csv_by_obj(char *buffer, int buflen, const char *name, const struct rusage *chrono) {
  double user_secs = TV_TO_SECS(chrono->ru_utime);
  double syst_secs = TV_TO_SECS(chrono->ru_stime);

  return snprintf(buffer, buflen, "%s, %f, %f, %f, %ld, %ld, %ld, %ld\n", name, user_secs + syst_secs, user_secs,
                  syst_secs, chrono->ru_majflt, chrono->ru_minflt, chrono->ru_inblock, chrono->ru_oublock);
}

void produce_chrono_report(char *buffer, int buflen) {
  if (!buffer || !buflen)
    return;
  uint32_t written = chrono_header_to_csv(buffer, buflen);
  if (written >= buflen)
    return;
  buffer += written;
  buflen -= written;

  for (uint32_t i = 0; i < __CHRONO_LAST__; ++i) {
    struct timeval utime = get_chrono(i)->ru_utime;
    if (!utime.tv_sec && !utime.tv_usec)
      continue;
    written = chrono_to_csv_by_id(buffer, buflen, i);

    if (written >= buflen)
      return;
    buffer += written;
    buflen -= written;
  }
}

/////////////////////////////////////////////////////////////////////////////////////////////////

void *get_function_by_name(const char *func_name) {
#ifdef __DYN_LINKING__
  void *symbol = dlsym(RTLD_DEFAULT, func_name);
  if (!symbol)
    LOG_ERROR("Cannot find symbol '%s' : %s", func_name, dlerror());
  return symbol;
#else
  ASSERT(0, "dynamic linking is not enabled");
  return NULL;
#endif
}
