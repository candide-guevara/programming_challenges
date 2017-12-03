#pragma once
#include <common.h>
#include <multi_swap_count.h>
#include <multi_swap_ctrl.h>
#include <other_swap_algo.h>

#include <pthread.h>
#include <stdint.h>

#ifndef BENCH_RERUN_COUNT
#define BENCH_RERUN_COUNT 10
#endif

#define ALL_IS_ALL 1
#define IS_ALL(func) PASTIFY(func, _IS_ALL)
#ifndef BENCH_FUNCTION
#define BENCH_FUNCTION bench_count_many_concurrent_readers
#define BENCH_FUNCTION_STR STRINGIFY(BENCH_FUNCTION)
#endif

#ifndef BENCH_FANCY_ALIGN
#define BENCH_FANCY_ALIGN 1
#endif
#ifndef BENCH_ALL_THR_COUNT
#define BENCH_ALL_THR_COUNT 4
#endif
#ifndef BENCH_ALL_THR_WAIT
#define BENCH_ALL_THR_WAIT 0
#endif
#ifndef BENCH_THREAD_ITERATIONS
#define BENCH_THREAD_ITERATIONS 10000
#endif

//////////////////////////////////////////////////////////////////////////////////////////////

struct BenchContent {
  uint64_t x, y, z;
#if BENCH_FANCY_ALIGN > 0
  ADD_CACHE_LINE_PADDING(3 * sizeof(uint64_t))
#endif
};

struct BenchThreadParams {
  MSWAP_Count *count;
  Swap_Mutex mutex;
  Swap_Spin spin;
  Swap_Rwlock rwlock;
  MSWAP_Stat *stats;
  struct BenchContent *src_dst;
  uint32_t thread_idx, wait_for, iterations;
  void *bench_specific;
};

typedef struct BenchContent BenchContent;
typedef struct BenchThreadParams BenchThreadParams;
typedef void *(*bench_thread_func)(void *);
typedef MSWAP_Stat (*bench_func_t)(BenchThreadParams *, uint32_t);

BenchThreadParams *build_bench_generic_params(uint32_t, uint32_t, uint32_t);
void free_bench_generic_params(BenchThreadParams *);
uint32_t write_bench_parameters(const char *, char *, uint32_t);
uint32_t write_timing_results(ChronoId, char *, uint32_t);
void print_bench_result_as_json(const char *, ChronoId, uint32_t);

MSWAP_Stat join_with_bench_threads(BenchThreadParams *, pthread_t *, uint32_t);
pthread_t *kick_off_bench_threads(BenchThreadParams *, uint32_t, bench_thread_func);

void bench_content_writer_func(void *, uint32_t, void *);
void bench_content_reader_func(void *, uint32_t, void *);

//////////////////////////////////////////////////////////////////////////////////////////////

void main_run_benchmark();
void run_all_benchmark();
void run_one_benchmark();

#define BENCH_SIMPLE_DECLARE(type_)                                                                                    \
  MSWAP_Stat bench_##type_##_many_concurrent_readers(BenchThreadParams *, uint32_t);                                   \
  MSWAP_Stat bench_##type_##_many_concurrent_writers(BenchThreadParams *, uint32_t);                                   \
  MSWAP_Stat bench_##type_##_many_concurrent_rd_wr(BenchThreadParams *, uint32_t);

#define BENCH_SIMPLE_TEMPLATE(type_)                                                                                   \
  MSWAP_Stat bench_##type_##_many_concurrent_readers(BenchThreadParams *params, uint32_t param_len) {                  \
    pthread_t *bench_thrs = kick_off_bench_threads(params, param_len, BENCH_SIMPLE_THREAD_FUNC_NAME(type_, read));     \
    MSWAP_Stat final_stats = join_with_bench_threads(params, bench_thrs, param_len);                                   \
    free(bench_thrs);                                                                                                  \
    return final_stats;                                                                                                \
  }                                                                                                                    \
                                                                                                                       \
  MSWAP_Stat bench_##type_##_many_concurrent_writers(BenchThreadParams *params, uint32_t param_len) {                  \
    pthread_t *bench_thrs = kick_off_bench_threads(params, param_len, BENCH_SIMPLE_THREAD_FUNC_NAME(type_, write));    \
    MSWAP_Stat final_stats = join_with_bench_threads(params, bench_thrs, param_len);                                   \
    free(bench_thrs);                                                                                                  \
    return final_stats;                                                                                                \
  }                                                                                                                    \
                                                                                                                       \
  MSWAP_Stat bench_##type_##_many_concurrent_rd_wr(BenchThreadParams *params, uint32_t param_len) {                    \
    uint32_t read_count = param_len / 2;                                                                               \
    uint32_t write_count = param_len - read_count;                                                                     \
    pthread_t *read_thrs = kick_off_bench_threads(params, read_count, BENCH_SIMPLE_THREAD_FUNC_NAME(type_, read));     \
    pthread_t *write_thrs                                                                                              \
        = kick_off_bench_threads(params + read_count, write_count, BENCH_SIMPLE_THREAD_FUNC_NAME(type_, write));       \
    MSWAP_Stat final_rd_stats = join_with_bench_threads(params, read_thrs, read_count);                                \
    MSWAP_Stat final_wr_stats = join_with_bench_threads(params + read_count, write_thrs, write_count);                 \
    free(read_thrs);                                                                                                   \
    free(write_thrs);                                                                                                  \
    mswap_stats_merge(&final_rd_stats, &final_wr_stats);                                                               \
    return final_rd_stats;                                                                                             \
  }

#define ALL_BENCHES_BODY(XX) XX(count), XX(mutex), XX(spin), XX(rwlock)
#define EXPAND_BENCH_NAMES(type_)                                                                                      \
  STRINGIFY(bench_##type_##_many_concurrent_readers)                                                                   \
  , STRINGIFY(bench_##type_##_many_concurrent_writers), STRINGIFY(bench_##type_##_many_concurrent_rd_wr)
#define EXPAND_BENCH_PTRS(type_)                                                                                       \
  bench_##type_##_many_concurrent_readers, bench_##type_##_many_concurrent_writers,                                    \
      bench_##type_##_many_concurrent_rd_wr

#define ENUMERATE_BENCHES(ptrs_, names_)                                                                               \
  const char *names_[] = { ALL_BENCHES_BODY(EXPAND_BENCH_NAMES) };                                                     \
  bench_func_t ptrs_[] = { ALL_BENCHES_BODY(EXPAND_BENCH_PTRS) };

BENCH_SIMPLE_DECLARE(count)
BENCH_SIMPLE_DECLARE(mutex)
BENCH_SIMPLE_DECLARE(spin)
BENCH_SIMPLE_DECLARE(rwlock)

//////////////////////////////////////////////////////////////////////////////////////////////

#define get_func_for_count(type_) bench_mt_content_##type_##_func
#define get_func_for_mutex(type_) bench_px_content_##type_##_func
#define get_func_for_spin(type_) bench_px_content_##type_##_func
#define get_func_for_rwlock(type_) bench_px_content_##type_##_func

#define BENCH_MT_THREAD_FUNC_TEMPLATE(type_)                                                                           \
  static void *bench_##type_##_thread_routine(void *params_raw) {                                                      \
    BenchThreadParams *params = (BenchThreadParams *)params_raw;                                                       \
    MSWAP_Count *mswap = params->mswap;                                                                                \
    CHRONO_START(RUSAGE_THREAD, CHRONO_THREAD_0 + params->thread_idx);                                                 \
    for (uint32_t iterations = params->iterations; iterations; --iterations) {                                         \
      MSWAP_Stat one_run_stat = count_mswap_##type_(mswap, params->src_dst, bench_mt_content_##type_##_func);          \
      mswap_stats_merge(params->stats, &one_run_stat);                                                                 \
      if (!one_run_stat.success)                                                                                       \
        pthread_yield();                                                                                               \
      else if (params->wait_for)                                                                                       \
        busy_spin_for(params->wait_for);                                                                               \
    }                                                                                                                  \
    CHRONO_STOP(RUSAGE_THREAD, CHRONO_THREAD_0 + params->thread_idx);                                                  \
    return params_raw;                                                                                                 \
  }

#define BENCH_SIMPLE_THREAD_FUNC_NAME(lock_, type_) bench_##lock_##_##type_##_thread_routine

#define BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(lock_, type_)                                                                \
  static void *bench_##lock_##_##type_##_thread_routine(void *params_raw) {                                            \
    BenchThreadParams *params = (BenchThreadParams *)params_raw;                                                       \
    CHRONO_START(RUSAGE_THREAD, CHRONO_THREAD_0 + params->thread_idx);                                                 \
    for (uint32_t iterations = params->iterations; iterations; --iterations) {                                         \
      lock_##_mswap_##type_(params->lock_, params->src_dst, get_func_for_##lock_(type_));                              \
      if (params->wait_for)                                                                                            \
        busy_spin_for(params->wait_for);                                                                               \
    }                                                                                                                  \
    CHRONO_STOP(RUSAGE_THREAD, CHRONO_THREAD_0 + params->thread_idx);                                                  \
    return params_raw;                                                                                                 \
  }

//////////////////////////////////////////////////////////////////////////////////////////////

static void bench_mt_content_read_func(void *restrict src, uint32_t slot, void *restrict dst) {
  BenchContent *src_content = (BenchContent *)src + slot;
  BenchContent *dst_content = (BenchContent *)dst;
  dst_content->x = src_content->x;
  dst_content->y = src_content->y;
  dst_content->z = src_content->z;
}

static void bench_mt_content_write_func(void *restrict src, uint32_t slot, void *restrict dst) {
  BenchContent *src_content = (BenchContent *)src + slot;
  BenchContent *dst_content = (BenchContent *)dst;
  src_content->x = dst_content->x;
  src_content->y = dst_content->y;
  src_content->z = dst_content->z;
}

static void bench_px_content_read_func(void *restrict src, void *restrict dst) {
  BenchContent *src_content = (BenchContent *)src;
  BenchContent *dst_content = (BenchContent *)dst;
  dst_content->x = src_content->x;
  dst_content->y = src_content->y;
  dst_content->z = src_content->z;
}

static void bench_px_content_write_func(void *restrict src, void *restrict dst) {
  bench_px_content_read_func(dst, src);
}

//////////////////////////////////////////////////////////////////////////////////////////////
