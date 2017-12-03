#pragma once

#define __STRINGIFY__(arg) #arg
#define STRINGIFY(arg) __STRINGIFY__(arg)

#define __PASTIFY__(arg1, arg2) arg1##arg2
#define PASTIFY(arg1, arg2) __PASTIFY__(arg1, arg2)

#ifndef __LEVEL_ASSERT__
#define __LEVEL_ASSERT__ 1
#endif

#if __LEVEL_ASSERT__ > 0
#define ASSERT(exp, msg, ...)                                                                                          \
  if (!(exp)) {                                                                                                        \
    __LOG__("ASSERT", msg " : " #exp, ##__VA_ARGS__);                                                                  \
    __my_assert__(!!(exp));                                                                                            \
  }
#else
#define ASSERT(exp, msg, ...)
#endif

#define TEST_ASSERT(exp, msg, ...)                                                                                     \
  if (!(exp)) {                                                                                                        \
    __LOG__("TEST_ASSERT", msg " : " #exp, ##__VA_ARGS__);                                                             \
    __my_assert__(!!(exp));                                                                                            \
  }

#define STATIC_ASSERT(COND, MSG) typedef char __static_assert_##MSG[(COND) ? 1 : -1]

void __my_assert__(int condition);

/////////////////////////////////////////////////////////////////////////////////////////////////

#define ENUM_VALUE(label) label
#define ENUM_NAME(label) #label

#define DECLARE_NAMED_ENUM(name, body)                                                                                 \
  typedef enum name name;                                                                                              \
  enum name { body(ENUM_VALUE) };                                                                                      \
  const char *name##_to_string(name);

#define DEFINE_ENUM_TO_STRING(name, body)                                                                              \
  const char *name##_to_string(name id) {                                                                              \
    static const char *labels[] = { body(ENUM_NAME) };                                                                 \
    if ((int)id < sizeof(labels) / sizeof(const char *))                                                               \
      return labels[id];                                                                                               \
    return NULL;                                                                                                       \
  }

/////////////////////////////////////////////////////////////////////////////////////////////////

#ifndef __LEVEL_DTRACE__
// you need to #include <sys/sdt.h>
#define __LEVEL_DTRACE__ 1
#endif

#ifndef __DTRACE_PROVIDER__
#define __DTRACE_PROVIDER__ project
#endif

// Cannot use __func__ because on some compilers it is defined as a variable and not as a macro
#if __LEVEL_DTRACE__ > 0
#define MY_DTRACE_PROBE(name) DTRACE_PROBE(__DTRACE_PROVIDER__, name)
#define MY_DTRACE_PROBE1(name, arg1) DTRACE_PROBE1(__DTRACE_PROVIDER__, name, arg1)
#define MY_DTRACE_PROBE2(name, arg1, arg2) DTRACE_PROBE2(__DTRACE_PROVIDER__, name, arg1, arg2)
#else
#define MY_DTRACE_PROBE(name)
#define MY_DTRACE_PROBE1(name, arg1)
#define MY_DTRACE_PROBE2(name, arg1, arg2)
#endif

/////////////////////////////////////////////////////////////////////////////////////////////////

#ifndef __LEVEL_CHRONO__
#define __LEVEL_CHRONO__ 1
#endif

#if __LEVEL_CHRONO__ > 0
#define CHRONO_START(chrono_who, chrono_id) __chrono_start__(chrono_who, chrono_id)
#define CHRONO_STOP(chrono_who, chrono_id) __chrono_stop__(chrono_who, chrono_id)
#else
#define CHRONO_START(chrono_who, chrono_id)
#define CHRONO_STOP(chrono_who, chrono_id)
#endif

// clang-format off
#define CHRONOID_ENUM(XX)  \
  XX(__CHRONO_FIRST__),    \
  XX(CHRONO_ALL_BENCH),    \
  XX(CHRONO_THREAD_0),     \
  XX(CHRONO_THREAD_1),     \
  XX(CHRONO_THREAD_2),     \
  XX(CHRONO_THREAD_3),     \
  XX(CHRONO_THREAD_4),     \
  XX(CHRONO_THREAD_5),     \
  XX(CHRONO_THREAD_6),     \
  XX(CHRONO_THREAD_7),     \
  XX(CHRONO_THREAD_8),     \
  XX(CHRONO_THREAD_9),     \
  XX(CHRONO_THREAD_10),    \
  XX(CHRONO_THREAD_11),    \
  XX(CHRONO_THREAD_12),    \
  XX(CHRONO_THREAD_13),    \
  XX(CHRONO_THREAD_14),    \
  XX(CHRONO_THREAD_15),    \
  XX(CHRONO_THREAD_16),    \
  XX(CHRONO_THREAD_17),    \
  XX(CHRONO_THREAD_18),    \
  XX(CHRONO_THREAD_19),    \
  XX(CHRONO_THREAD_20),    \
  XX(CHRONO_THREAD_21),    \
  XX(CHRONO_THREAD_22),    \
  XX(CHRONO_THREAD_23),    \
  XX(CHRONO_THREAD_24),    \
  XX(__CHRONO_LAST__),
// clang-format on

#define TV_TO_SECS(elapsed) (double)(elapsed).tv_sec + (double)((elapsed).tv_usec) / 1000000
#define TS_TO_SECS(elapsed) (double)(elapsed).tv_sec + (double)((elapsed).tv_nsec) / 1000000000

DECLARE_NAMED_ENUM(ChronoId, CHRONOID_ENUM)
struct rusage;

void __chrono_start__(int, ChronoId);
void __chrono_stop__(int, ChronoId);

const struct rusage *get_chrono(ChronoId);
const struct timespec *get_real_time(ChronoId);
void diff_chrono_by_id(struct rusage *, ChronoId, ChronoId);
void diff_chrono_by_obj(struct rusage *, const struct rusage *, const struct rusage *);
void clear_chrono_by_id(ChronoId);
void clear_all_chrono();
void clear_chrono_by_obj(struct rusage *, struct timespec *); 

int chrono_header_to_csv(char *, int);
int chrono_to_csv_by_id(char *, int, ChronoId);
int chrono_to_csv_by_obj(char *, int, const char *, const struct rusage *);
void produce_chrono_report(char *, int);

/////////////////////////////////////////////////////////////////////////////////////////////////

#define is_null_or_empty(str) (str) == NULL || *(str) == '\0'

// you need to linker flags -ldl and -rdynamic (dlsym only finds symbols in the dynamic table)
void *get_function_by_name(const char *);

/////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef __clang__
#define __COMPILER__ clang
#elif defined(__GNUC__)
#define __COMPILER__ GCC
#else
#define __COMPILER__ CouldNotDetectCompiler
#endif

#ifdef __has_feature
#if __has_feature(thread_sanitizer)
#define __SANITIZER_THREAD_ON__
#endif
#endif

#define IGNORE_WARNING_PUSH(flag)                                                                                      \
  _Pragma(STRINGIFY(__COMPILER__ diagnostic push)) _Pragma(STRINGIFY(__COMPILER__ diagnostic ignored flag))

#define IGNORE_WARNING_POP _Pragma(STRINGIFY(__COMPILER__ diagnostic pop))
