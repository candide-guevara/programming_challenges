#include <benchmark.h>

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>

#include <common.h>
#include <logger.h>
#include <multi_swap_count.h>
#include <util.h>

void main_run_benchmark() {
  const uint32_t buf_init_len = 1024 * 1024;
  char *buffer = calloc(1, buf_init_len);
  BenchThreadParams *params
      = build_bench_generic_params(BENCH_ALL_THR_COUNT, BENCH_ALL_THR_WAIT, BENCH_THREAD_ITERATIONS);

  CHRONO_START(RUSAGE_SELF, __CHRONO_FIRST__);
  for (uint32_t it = 0; it < BENCH_RERUN_COUNT; ++it)
    BENCH_FUNCTION(params, BENCH_ALL_THR_COUNT);
  CHRONO_STOP(RUSAGE_SELF, __CHRONO_FIRST__);

  free_bench_generic_params(params);

  uint32_t written = write_bench_parameters(buffer, buf_init_len);
  write_timing_results(__CHRONO_FIRST__, buffer+written, buf_init_len-written);
  printf("%s", buffer);
  free(buffer);
}

void main_run_benchmark_instrumented() {
  const uint32_t buf_init_len = 1024 * 1024;
  char *time_buffer = calloc(1, buf_init_len);
  char *stat_buffer = calloc(1, buf_init_len);
  BenchThreadParams *params
      = build_bench_generic_params(BENCH_ALL_THR_COUNT, BENCH_ALL_THR_WAIT, BENCH_THREAD_ITERATIONS);

  CHRONO_START(RUSAGE_SELF, __CHRONO_FIRST__);
  for (uint32_t it = 0, buf_start = 0, buf_len = buf_init_len; //
       it < BENCH_RERUN_COUNT && buf_len <= buf_init_len;      //
       ++it, buf_len -= buf_start)
    buf_start += BENCH_FUNCTION_INST(params, BENCH_ALL_THR_COUNT, stat_buffer + buf_start, buf_len);
  CHRONO_STOP(RUSAGE_SELF, __CHRONO_FIRST__);

  produce_chrono_report(time_buffer, buf_init_len);
  LOG_WARN("Results : \n%s\n%s", time_buffer, stat_buffer);
  free_bench_generic_params(params);
  free(stat_buffer);
  free(time_buffer);
}

//////////////////////////////////////////////////////////////////////////////////////////////

BENCH_MT_THREAD_FUNC_TEMPLATE(read)
BENCH_MT_THREAD_FUNC_TEMPLATE(write)
BENCH_PX_THREAD_FUNC_TEMPLATE(mutex, read)
BENCH_PX_THREAD_FUNC_TEMPLATE(mutex, write)
BENCH_PX_THREAD_FUNC_TEMPLATE(spin, read)
BENCH_PX_THREAD_FUNC_TEMPLATE(spin, write)
BENCH_PX_THREAD_FUNC_TEMPLATE(rwlock, read)
BENCH_PX_THREAD_FUNC_TEMPLATE(rwlock, write)

#define BENCH_SIMPLE_TEMPLATE(type_, read_func_, write_func_)                                                          \
  MSWAP_Stat bench_##type_##_many_concurrent_readers(BenchThreadParams *params, uint32_t param_len) {                  \
    pthread_t *bench_thrs = kick_off_bench_threads(params, param_len, read_func_);                                     \
    MSWAP_Stat final_stats = join_with_bench_threads(params, bench_thrs, param_len);                                   \
    free(bench_thrs);                                                                                                  \
    return final_stats;                                                                                                \
  }                                                                                                                    \
                                                                                                                       \
  MSWAP_Stat bench_##type_##_many_concurrent_writers(BenchThreadParams *params, uint32_t param_len) {                  \
    pthread_t *bench_thrs = kick_off_bench_threads(params, param_len, write_func_);                                    \
    MSWAP_Stat final_stats = join_with_bench_threads(params, bench_thrs, param_len);                                   \
    free(bench_thrs);                                                                                                  \
    return final_stats;                                                                                                \
  }                                                                                                                    \
                                                                                                                       \
  MSWAP_Stat bench_##type_##_many_concurrent_rd_wr(BenchThreadParams *params, uint32_t param_len) {                    \
    uint32_t read_count = param_len / 2;                                                                               \
    uint32_t write_count = param_len - param_len / 2;                                                                  \
    pthread_t *read_thrs = kick_off_bench_threads(params, read_count, read_func_);                                     \
    pthread_t *write_thrs = kick_off_bench_threads(params + read_count, write_count, write_func_);                     \
    MSWAP_Stat final_rd_stats = join_with_bench_threads(params, read_thrs, read_count);                                \
    MSWAP_Stat final_wr_stats = join_with_bench_threads(params + read_count, write_thrs, write_count);                 \
    free(read_thrs);                                                                                                   \
    free(write_thrs);                                                                                                  \
    mswap_stats_merge(&final_rd_stats, &final_wr_stats);                                                               \
    return final_rd_stats;                                                                                             \
  }

BENCH_SIMPLE_TEMPLATE(mswap, bench_read_thread_routine, bench_write_thread_routine)
BENCH_SIMPLE_TEMPLATE(mutex, bench_mutex_read_thread_routine, bench_mutex_write_thread_routine)
BENCH_SIMPLE_TEMPLATE(spin, bench_spin_read_thread_routine, bench_spin_write_thread_routine)
BENCH_SIMPLE_TEMPLATE(rwlock, bench_rwlock_read_thread_routine, bench_rwlock_write_thread_routine)

//////////////////////////////////////////////////////////////////////////////////////////////

uint32_t bench_mswap_instrumented_rd_wr(BenchThreadParams *params, uint32_t param_len, char *buffer,
                                        uint32_t buffer_len) {
  return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////

BenchThreadParams *build_bench_generic_params(uint32_t param_len, uint32_t wait_for, uint32_t iterations) {
  BenchThreadParams *params = calloc(param_len, sizeof(BenchThreadParams));

  const size_t content_slots_size = SWCOUNT_SLOTS * sizeof(BenchContent);
  const size_t src_dst_size = param_len * sizeof(BenchContent);
  const size_t stats_size = param_len * sizeof(MSWAP_Stat);

#if BENCH_FANCY_ALIGN > 0
  BenchContent *content = aligned_alloc(CACHE_LINE_SIZE, content_slots_size);
  BenchContent *src_dst = aligned_alloc(CACHE_LINE_SIZE, src_dst_size);
  MSWAP_Stat *stats = aligned_alloc(CACHE_LINE_SIZE, stats_size);
  MSWAP_Count *mswap = aligned_alloc(CACHE_LINE_SIZE, sizeof(MSWAP_Count));
#else
  BenchContent *src_dst = malloc(src_dst_size);
  BenchContent *content = malloc(content_slots_size);
  MSWAP_Stat *stats = malloc(stats_size);
  MSWAP_Count *mswap = malloc(sizeof(MSWAP_Count));
#endif

  *mswap = build_mswap_count(content);
  Swap_Mutex mutex = mutex_swap_init(content);
  Swap_Spin spin = spin_swap_init(content);
  Swap_Rwlock rwlock = rwlock_swap_init(content);

  // memset no needed when using calloc, but memalign does not zero out
  memset(content, 0, content_slots_size);
  memset(src_dst, 0, src_dst_size);
  memset(stats, 0, stats_size);

  for (uint32_t i = 0; i < param_len; ++i) {
    params[i].mswap = mswap;
    params[i].mutex = mutex;
    params[i].spin = spin;
    params[i].rwlock = rwlock;
    params[i].src_dst = src_dst + i;
    params[i].stats = stats + i;
    params[i].wait_for = WAIT_QUANTUM * wait_for;
    params[i].iterations = iterations;
  }
  return params;
}

pthread_t *kick_off_bench_threads(BenchThreadParams *params, uint32_t param_len, bench_thread_func func) {
  pthread_t *bench_thrs = calloc(param_len, sizeof(pthread_t));
  FOREACH_IN_ARRAY(bench_thrs, bench_thr, param_len) {
    BenchThreadParams *param = params + idx;
    int creation_ok = pthread_create(bench_thr, NULL, func, param);
    TEST_ASSERT(creation_ok == 0, "Failed to create bench thread %d", idx);
  }
  return bench_thrs;
}

MSWAP_Stat join_with_bench_threads(BenchThreadParams *params, pthread_t *bench_thrs, uint32_t param_len) {
  uint32_t wait_millis = 666 + BENCH_THREAD_ITERATIONS;
  int join_ok = join_all(bench_thrs, param_len, wait_millis);
  TEST_ASSERT(join_ok == 0, "Bench threads never completed");
  MSWAP_Stat final_stats = { 0 };
  FOREACH_IN_ARRAY(params, param, param_len) { mswap_stats_merge(&final_stats, param->stats); }
  return final_stats;
}

void free_bench_generic_params(BenchThreadParams *params) {
  free(params->stats);
  free(params->src_dst);
  free_mswap_count(params->mswap);
  rwlock_swap_clean(params->rwlock);
  spin_swap_clean(params->spin);
  mutex_swap_clean(params->mutex);
  free(params->mswap);
  free(params);
}

uint32_t write_bench_parameters(char *buffer, uint32_t buf_len) {
  const char *format_msg = "BENCH_RERUN_COUNT = %d\n"
                           "BENCH_FUNCTION = %s\n"
                           "BENCH_FUNCTION_INST = %s\n"
                           "BENCH_FANCY_ALIGN = %d\n"
                           "BENCH_ALL_THR_COUNT = %d\n"
                           "BENCH_ALL_THR_WAIT = %d\n"
                           "BENCH_THREAD_ITERATIONS = %d\n"
                           "MAX_ACQUIRE_SWAP_ATTEMPTS = %d\n"
                           "MAX_RELEASE_SWAP_ATTEMPTS = %d\n"
                           "WAIT_QUANTUM = %d\n"
                           "MORE_ATO_LESS_CONTENTION = %d\n"
                           "MEMORY_ORDER_REL_LOCK = %d\n"
                           "MEMORY_ORDER_ACQ_LOCK = %d\n"
                           "\n";
  return snprintf(buffer, buf_len, format_msg, BENCH_RERUN_COUNT, STRINGIFY(BENCH_FUNCTION),
                  STRINGIFY(BENCH_FUNCTION_INST), BENCH_FANCY_ALIGN, BENCH_ALL_THR_COUNT, BENCH_ALL_THR_WAIT,
                  BENCH_THREAD_ITERATIONS, MAX_ACQUIRE_SWAP_ATTEMPTS, MAX_RELEASE_SWAP_ATTEMPTS, WAIT_QUANTUM,
                  MORE_ATO_LESS_CONTENTION, MEMORY_ORDER_REL_LOCK, MEMORY_ORDER_ACQ_LOCK);
}

uint32_t write_timing_results(ChronoId chrono_id, char *buffer, uint32_t buf_len) {
  const struct rusage *chrono = get_chrono(chrono_id);
  const struct timespec *time = get_real_time(chrono_id);
  double user_secs = TV_TO_SECS(chrono->ru_utime);
  double syst_secs = TV_TO_SECS(chrono->ru_stime);
  double real_secs = TS_TO_SECS(*time);
  return snprintf(buffer, buf_len, "user_secs = %f\nsyst_secs = %f\nreal_secs = %f\n\n", 
  user_secs, syst_secs, real_secs);
}

//////////////////////////////////////////////////////////////////////////////////////////////
