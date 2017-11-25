#pragma once
#include <multi_cas_ctrl.h>
#include <multi_cas_noaba.h>

#include <stdint.h>
#include <pthread.h>

#ifndef BENCH_ITERATIONS
#define BENCH_ITERATIONS 100
#endif
#ifndef BENCH_FUNCTION
#define BENCH_FUNCTION bench_mcas_many_concurrent_readers
#endif
#ifndef BENCH_FANCY_ALIGN
#define BENCH_FANCY_ALIGN 0
#endif
#ifndef BENCH_READER_COUNT
#define BENCH_READER_COUNT 6
#endif
#ifndef BENCH_READER_WAIT
#define BENCH_READER_WAIT 0
#endif
#ifndef BENCH_READ_ITERATIONS
#define BENCH_READ_ITERATIONS 100000
#endif

//////////////////////////////////////////////////////////////////////////////////////////////

struct BenchContent {
  uint64_t x, y, z;
#if BENCH_FANCY_ALIGN > 0
  ADD_CACHE_LINE_PADDING(3 * sizeof(uint64_t))
#endif
};

struct BenchThreadParams {
  MCas_NoABA *mcas;
  MCAS_Stat *stats;
  struct BenchContent *read_dst;
  uint32_t wait_for, iterations;
  void *bench_specific;
};

typedef struct BenchContent BenchContent;
typedef struct BenchThreadParams BenchThreadParams;
typedef void *(*bench_thread_func)(void *);

BenchThreadParams *build_bench_generic_params(uint32_t, uint32_t, uint32_t);
void free_bench_generic_params(BenchThreadParams *);

MCAS_Stat join_with_reader_threads(BenchThreadParams *, pthread_t *, uint32_t);
pthread_t *kick_off_bench_reader_threads(BenchThreadParams *, uint32_t, bench_thread_func);

void bench_content_writer_func(void *, uint32_t, void *);
void bench_content_reader_func(void *, uint32_t, void *);

//////////////////////////////////////////////////////////////////////////////////////////////

void main_run_benchmark();
void *bench_reader_thread_routine(void *);

uint32_t bench_mcas_many_concurrent_readers(BenchThreadParams *, uint32_t, char*, uint32_t);

