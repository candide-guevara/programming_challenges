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
#if IS_ALL(BENCH_FUNCTION)
  run_all_benchmark();
#else
  run_one_benchmark();
#endif
}

//////////////////////////////////////////////////////////////////////////////////////////////

void run_one_benchmark() {
#if IS_ALL(BENCH_FUNCTION) != 1
  BenchThreadParams *params
      = build_bench_generic_params(BENCH_ALL_THR_COUNT, BENCH_ALL_THR_WAIT, BENCH_THREAD_ITERATIONS);

  CHRONO_START(RUSAGE_SELF, CHRONO_ALL_BENCH);
  for (uint32_t it = 0; it < BENCH_RERUN_COUNT; ++it)
    BENCH_FUNCTION(params, BENCH_ALL_THR_COUNT);
  CHRONO_STOP(RUSAGE_SELF, CHRONO_ALL_BENCH);

  free_bench_generic_params(params);
  print_bench_result_as_json(BENCH_FUNCTION_STR, CHRONO_ALL_BENCH, BENCH_ALL_THR_COUNT);
#endif
}

void run_all_benchmark() {
  ENUMERATE_BENCHES(func_ptrs, func_names);
  BenchThreadParams *params
      = build_bench_generic_params(BENCH_ALL_THR_COUNT, BENCH_ALL_THR_WAIT, BENCH_THREAD_ITERATIONS);

  for (uint32_t i = 0; i < sizeof(func_ptrs) / sizeof(void *); ++i) {
    CHRONO_START(RUSAGE_SELF, CHRONO_ALL_BENCH);
    for (uint32_t it = 0; it < BENCH_RERUN_COUNT; ++it)
      func_ptrs[i](params, BENCH_ALL_THR_COUNT);
    CHRONO_STOP(RUSAGE_SELF, CHRONO_ALL_BENCH);

    print_bench_result_as_json(func_names[i], CHRONO_ALL_BENCH, BENCH_ALL_THR_COUNT);
    clear_all_chrono();
  }

  free_bench_generic_params(params);
}

//////////////////////////////////////////////////////////////////////////////////////////////

BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(count, read)
BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(count, write)
BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(mutex, read)
BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(mutex, write)
BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(spin, read)
BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(spin, write)
BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(rwlock, read)
BENCH_SIMPLE_THREAD_FUNC_TEMPLATE(rwlock, write)

BENCH_SIMPLE_TEMPLATE(count)
BENCH_SIMPLE_TEMPLATE(mutex)
BENCH_SIMPLE_TEMPLATE(spin)
BENCH_SIMPLE_TEMPLATE(rwlock)

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
  Swap_Mutex mutex = mutex_mswap_init(content);
  Swap_Spin spin = spin_mswap_init(content);
  Swap_Rwlock rwlock = rwlock_mswap_init(content);

  // memset no needed when using calloc, but memalign does not zero out
  memset(content, 0, content_slots_size);
  memset(src_dst, 0, src_dst_size);
  memset(stats, 0, stats_size);

  for (uint32_t i = 0; i < param_len; ++i) {
    params[i].count = mswap;
    params[i].mutex = mutex;
    params[i].spin = spin;
    params[i].rwlock = rwlock;
    params[i].src_dst = src_dst + i;
    params[i].stats = stats + i;
    params[i].wait_for = WAIT_QUANTUM * wait_for;
    params[i].iterations = iterations;
    params[i].thread_idx = i;
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
  free_mswap_count(params->count);
  rwlock_mswap_clean(params->rwlock);
  spin_mswap_clean(params->spin);
  mutex_mswap_clean(params->mutex);
  free(params->count);
  free(params);
}

uint32_t write_bench_parameters(const char *func_name, char *buffer, uint32_t buf_len) {
  const char *format_msg = "{\"BENCH_RERUN_COUNT\":%d,"
                           "\"BENCH_FUNCTION\":\"%s\","
                           "\"BENCH_FANCY_ALIGN\":%d,"
                           "\"BENCH_ALL_THR_COUNT\":%d,"
                           "\"BENCH_ALL_THR_WAIT\":%d,"
                           "\"BENCH_THREAD_ITERATIONS\":%d,"
                           "\"MAX_ACQUIRE_SWAP_ATTEMPTS\":%d,"
                           "\"MAX_RELEASE_SWAP_ATTEMPTS\":%d,"
                           "\"WAIT_QUANTUM\":%d,"
                           "\"MORE_ATO_LESS_CONTENTION\":%d,"
                           "\"MEMORY_ORDER_REL_LOCK\":\"%s\","
                           "\"MEMORY_ORDER_ACQ_LOCK\":\"%s\"}";
  return snprintf(buffer, buf_len, format_msg, BENCH_RERUN_COUNT, func_name,
                  BENCH_FANCY_ALIGN, BENCH_ALL_THR_COUNT, BENCH_ALL_THR_WAIT, BENCH_THREAD_ITERATIONS,
                  MAX_ACQUIRE_SWAP_ATTEMPTS, MAX_RELEASE_SWAP_ATTEMPTS, WAIT_QUANTUM, MORE_ATO_LESS_CONTENTION,
                  STRINGIFY(MEMORY_ORDER_REL_LOCK), STRINGIFY(MEMORY_ORDER_ACQ_LOCK));
}

uint32_t write_timing_results(ChronoId chrono_id, char *buffer, uint32_t buf_len) {
  const struct rusage *chrono = get_chrono(chrono_id);
  const struct timespec *time = get_real_time(chrono_id);
  double user_secs = TV_TO_SECS(chrono->ru_utime);
  double syst_secs = TV_TO_SECS(chrono->ru_stime);
  double real_secs = TS_TO_SECS(*time);
  return snprintf(buffer, buf_len, "{\"user_secs\":%f,\"syst_secs\":%f,\"real_secs\":%f}", user_secs, syst_secs,
                  real_secs);
}

void print_bench_result_as_json(const char *func_name, ChronoId chrono_id, uint32_t thread_len) {
  char param_buf[4096], result_buf[4096], main_res_buf[1024];
  write_bench_parameters(func_name, param_buf, sizeof(param_buf));
  write_timing_results(chrono_id, main_res_buf, sizeof(main_res_buf));

  uint32_t written = 0;
  for (uint32_t i = 0; i < thread_len; ++i) {
    written += write_timing_results(CHRONO_THREAD_0 + i, result_buf + written, sizeof(result_buf) - written);
    result_buf[written++] = ',';
  }
  result_buf[--written] = '\0';
  printf("{\"main\":%s,\"results\":[%s],\"params\":%s}\n", main_res_buf, result_buf, param_buf);
}

//////////////////////////////////////////////////////////////////////////////////////////////
