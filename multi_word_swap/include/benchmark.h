#pragma once
#include <multi_swap_ctrl.h>
#include <multi_swap_count.h>
#include <other_swap_algo.h>
#include <common.h>

#include <pthread.h>
#include <stdint.h>

#ifndef BENCH_RERUN_COUNT
#define BENCH_RERUN_COUNT 100
#endif
#ifndef BENCH_FUNCTION
#define BENCH_FUNCTION bench_spin_many_concurrent_rd_wr
#endif
#ifndef BENCH_FUNCTION_INST
#define BENCH_FUNCTION_INST bench_mswap_instrumented_rd_wr
#endif
#ifndef BENCH_FANCY_ALIGN
#define BENCH_FANCY_ALIGN 1
#endif
#ifndef BENCH_ALL_THR_COUNT
#define BENCH_ALL_THR_COUNT 6
#endif
#ifndef BENCH_ALL_THR_WAIT
#define BENCH_ALL_THR_WAIT 0
#endif
#ifndef BENCH_THREAD_ITERATIONS
#define BENCH_THREAD_ITERATIONS 100000
#endif

//////////////////////////////////////////////////////////////////////////////////////////////

struct BenchContent {
  uint64_t x, y, z;
#if BENCH_FANCY_ALIGN > 0
  ADD_CACHE_LINE_PADDING(3 * sizeof(uint64_t))
#endif
};

struct BenchThreadParams {
  MSWAP_Count *mswap;
  Swap_Mutex mutex;
  Swap_Spin spin;
  Swap_Rwlock rwlock;
  MSWAP_Stat *stats;
  struct BenchContent *src_dst;
  uint32_t wait_for, iterations;
  void *bench_specific;
};

typedef struct BenchContent BenchContent;
typedef struct BenchThreadParams BenchThreadParams;
typedef void *(*bench_thread_func)(void *);

BenchThreadParams *build_bench_generic_params(uint32_t, uint32_t, uint32_t);
void free_bench_generic_params(BenchThreadParams *);
uint32_t write_bench_parameters(char*, uint32_t);
uint32_t write_timing_results(ChronoId, char*, uint32_t);

MSWAP_Stat join_with_bench_threads(BenchThreadParams *, pthread_t *, uint32_t);
pthread_t *kick_off_bench_threads(BenchThreadParams *, uint32_t, bench_thread_func);

void bench_content_writer_func(void *, uint32_t, void *);
void bench_content_reader_func(void *, uint32_t, void *);

//////////////////////////////////////////////////////////////////////////////////////////////

void main_run_benchmark();
void main_run_benchmark_instrumented();

uint32_t bench_mswap_instrumented_rd_wr(BenchThreadParams *, uint32_t, char *, uint32_t);

MSWAP_Stat bench_mswap_many_concurrent_readers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_mswap_many_concurrent_writers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_mswap_many_concurrent_rd_wr(BenchThreadParams *, uint32_t);

MSWAP_Stat bench_mutex_many_concurrent_readers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_mutex_many_concurrent_writers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_mutex_many_concurrent_rd_wr(BenchThreadParams *, uint32_t);

MSWAP_Stat bench_spin_many_concurrent_readers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_spin_many_concurrent_writers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_spin_many_concurrent_rd_wr(BenchThreadParams *, uint32_t);

MSWAP_Stat bench_rwlock_many_concurrent_readers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_rwlock_many_concurrent_writers(BenchThreadParams *, uint32_t);
MSWAP_Stat bench_rwlock_many_concurrent_rd_wr(BenchThreadParams *, uint32_t);

//////////////////////////////////////////////////////////////////////////////////////////////

#define BENCH_MT_THREAD_FUNC_TEMPLATE(type_)                                                                           \
  static void *bench_##type_##_thread_routine(void *params_raw) {                                                      \
    BenchThreadParams *params = (BenchThreadParams *)params_raw;                                                       \
    MSWAP_Count *mswap = params->mswap;                                                                                \
    for (uint32_t iterations = params->iterations; iterations; --iterations) {                                         \
      MSWAP_Stat one_run_stat = count_mswap_##type_(mswap, params->src_dst, bench_mt_content_##type_##_func);           \
      mswap_stats_merge(params->stats, &one_run_stat);                                                                 \
      if (!one_run_stat.success)                                                                                       \
        pthread_yield();                                                                                               \
      else if (params->wait_for)                                                                                       \
        busy_spin_for(params->wait_for);                                                                               \
    }                                                                                                                  \
    return params_raw;                                                                                                 \
  }

#define BENCH_PX_THREAD_FUNC_TEMPLATE(lock_, type_)                                                                    \
  static void *bench_##lock_##_##type_##_thread_routine(void *params_raw) {                                            \
    BenchThreadParams *params = (BenchThreadParams *)params_raw;                                                       \
    while (params->iterations--) {                                                                                     \
      lock_##_swap_##type_(params->lock_, params->src_dst, bench_px_content_##type_##_func);                           \
      if (params->wait_for)                                                                                            \
        busy_spin_for(params->wait_for);                                                                               \
    }                                                                                                                  \
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
