#include <benchmark.h>

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>

#include <common.h>
#include <logger.h>
#include <multi_cas_noaba.h>
#include <util.h>

void main_run_benchmark() {
  LOG_WARN("Start bench : " STRINGIFY(BENCH_FUNCTION));
  const uint32_t buf_init_len = 1024 * 1024;
  char *time_buffer = calloc(1, buf_init_len);
  char *stat_buffer = calloc(1, buf_init_len);
  BenchThreadParams *params = build_bench_generic_params(BENCH_READER_COUNT, BENCH_READER_WAIT, BENCH_ITERATIONS);

  CHRONO_START(RUSAGE_SELF, __CHRONO_FIRST__);
  for (uint32_t it = 0, buf_start = 0, buf_len = buf_init_len; //
       it < BENCH_ITERATIONS && buf_len <= buf_init_len;       //
       ++it, buf_len -= buf_start)
    buf_start += BENCH_FUNCTION(params, BENCH_READER_COUNT, stat_buffer + buf_start, buf_len);
  CHRONO_STOP(RUSAGE_SELF, __CHRONO_FIRST__);

  produce_chrono_report(time_buffer, buf_init_len);
  LOG_WARN("Results : \n%s\n%s", time_buffer, stat_buffer);
  free_bench_generic_params(params);
  free(stat_buffer);
  free(time_buffer);
}

//////////////////////////////////////////////////////////////////////////////////////////////

uint32_t bench_mcas_many_concurrent_readers(BenchThreadParams *params, uint32_t param_len, char *buffer,
                                            uint32_t buf_len) {
  pthread_t *reader_thrs = kick_off_bench_reader_threads(params, param_len, bench_reader_thread_routine);
  MCAS_Stat final_stats = join_with_reader_threads(params, reader_thrs, param_len);
  free(reader_thrs);
  uint32_t written = mcas_stats_print(buffer, final_stats);
  return written;
}

//////////////////////////////////////////////////////////////////////////////////////////////

void bench_content_writer_func(void *restrict src, uint32_t slot, void *restrict dst) {
  bench_content_reader_func(dst, slot, src);
}

void bench_content_reader_func(void *restrict src, uint32_t slot, void *restrict dst) {
  BenchContent *src_content = (BenchContent *)src + slot;
  BenchContent *dst_content = (BenchContent *)dst;
  dst_content->x = src_content->x;
  dst_content->y = src_content->y;
  dst_content->z = src_content->z;
}

void *bench_reader_thread_routine(void *params_raw) {
  BenchThreadParams *params = (BenchThreadParams *)params_raw;
  MCas_NoABA *mcas = params->mcas;

  for (uint32_t reader_its = params->iterations; reader_its; --reader_its) {
    MCAS_Stat one_run_stat = noaba_cas_read(mcas, params->read_dst, bench_content_reader_func);
    mcas_stats_merge(params->stats, &one_run_stat);

    if (!one_run_stat.success) {
      pthread_yield();
      if (one_run_stat.to_drain)
        purge_all_version_locks(&mcas->control);
    }
    else if (params->wait_for)
      busy_spin_for(params->wait_for);
  }
  return params_raw;
}

//////////////////////////////////////////////////////////////////////////////////////////////

BenchThreadParams *build_bench_generic_params(uint32_t param_len, uint32_t wait_for, uint32_t iterations) {
  BenchThreadParams *params = calloc(param_len, sizeof(BenchThreadParams));

  const size_t content_slots_size = NOABA_SLOTS * sizeof(BenchContent);
  const size_t read_dst_size = param_len * sizeof(BenchContent);
  const size_t stats_size = param_len * sizeof(MCAS_Stat);

#if BENCH_FANCY_ALIGN > 0
  BenchContent *content = aligned_alloc(CACHE_LINE_SIZE, content_slots_size);
  BenchContent *read_dst = aligned_alloc(CACHE_LINE_SIZE, read_dst_size);
  MCAS_Stat *stats = aligned_alloc(CACHE_LINE_SIZE, stats_size) MCas_NoABA *mcas
      = aligned_alloc(CACHE_LINE_SIZE, sizeof(MCas_NoABA));
#else
  BenchContent *read_dst = malloc(read_dst_size);
  BenchContent *content = malloc(content_slots_size);
  MCAS_Stat *stats = malloc(stats_size);
  MCas_NoABA *mcas = malloc(sizeof(MCas_NoABA));
#endif

  *mcas = build_mcas_noaba(content);
  // memset no needed when using calloc, but memalign does not zero out
  memset(content, 0, content_slots_size);
  memset(read_dst, 0, read_dst_size);
  memset(stats, 0, stats_size);

  for (uint32_t i = 0; i < param_len; ++i) {
    params[i].mcas = mcas;
    params[i].read_dst = read_dst + i;
    params[i].stats = stats + i;
    params[i].wait_for = WAIT_QUANTUM * wait_for;
    params[i].iterations = iterations;
  }
  return params;
}

pthread_t *kick_off_bench_reader_threads(BenchThreadParams *params, uint32_t param_len, bench_thread_func func) {
  pthread_t *reader_thrs = calloc(param_len, sizeof(pthread_t));
  FOREACH_IN_ARRAY(reader_thrs, reader_thr, param_len) {
    BenchThreadParams *param = params + idx;
    int creation_ok = pthread_create(reader_thr, NULL, func, param);
    TEST_ASSERT(creation_ok == 0, "Failed to create reader thread %d", idx);
  }
  return reader_thrs;
}

MCAS_Stat join_with_reader_threads(BenchThreadParams *params, pthread_t *reader_thrs, uint32_t param_len) {
  uint32_t wait_millis = 666 + params[0].wait_for * param_len;
  int join_ok = join_all(reader_thrs, param_len, wait_millis);
  TEST_ASSERT(join_ok == 0, "Reader threads never completed");
  MCAS_Stat final_stats = { 0 };
  FOREACH_IN_ARRAY(params, param, param_len) { mcas_stats_merge(&final_stats, param->stats); }
  return final_stats;
}

void free_bench_generic_params(BenchThreadParams *params) {
  free(params->stats);
  free(params->read_dst);
  free_mcas_noaba(params->mcas);
  free(params->mcas);
  free(params);
}
