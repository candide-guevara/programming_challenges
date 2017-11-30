#pragma once
#include "multi_swap_ctrl.h"
#include "other_swap_algo.h"

#include <pthread.h>

#define UNIT_TEST_START(test_name)                                                                                     \
  void test_name() {                                                                                                   \
    LOG_WARN(STRINGIFY(test_name) " start");

#define UNIT_TEST_END }

#define SET_READ_LOCK_AND_SLOT(view, l, s, v)                                                                          \
  {                                                                                                                    \
    LockDesc desc = {.lock = l, .slot = s, .version = v };                                                             \
    set_version_lock(view, desc);                                                                                      \
  }

#define MT_MSWAP_BOILER_PLATE(max_mswap_thr_, thr_iterations_, mswap_, mswap_dst_, stats_, params_)                    \
  const uint32_t max_mswap_thr = max_mswap_thr_;                                                                       \
  const uint32_t mswap_rounds = thr_iterations_;                                                                       \
  TestContentSlots content_ = {[0] = { 0, 0, 0 }, [1] = { 1, 1, 1 }, [2] = { 2, 2, 2 }, [3] = { 3, 3, 3 } };           \
  MSWAP_Stat stats_[max_mswap_thr_] = { { 0 } };                                                                       \
  HelperParams params_[max_mswap_thr_];                                                                                \
  MSWAP_Count mswap_ = build_mswap_count(content_);                                                                    \
  TestContent mswap_dst_[max_mswap_thr_] = {[0 ... max_mswap_thr_ - 1] = {.x = 111, .y = 222, .z = 333 } };            \
  for (uint32_t i = 0; i < max_mswap_thr_; ++i) {                                                                      \
    params_[i].mswap = &mswap_;                                                                                        \
    params_[i].target = mswap_dst_ + i;                                                                                \
    params_[i].stats = stats_ + i;                                                                                     \
    params_[i].thr_iterations = mswap_rounds;                                                                          \
    params_[i].wait_for = 0;                                                                                           \
  }

#define MT_KICKOFF_MSWAP_THREADS(mswap_thr_, mswap_dst_, stats_, params_, mswap_func_)                                 \
  pthread_t mswap_thr_[max_mswap_thr];                                                                                 \
  FOREACH_MSWAP_THREAD(thr, mswap_dst_, stats_, params_) {                                                             \
    int creation_ok = pthread_create(mswap_thr_ + thr, NULL, mswap_func_, PASTIFY(params_, _i));                       \
    TEST_ASSERT(creation_ok == 0, "Failed to create reader thread %d", thr);                                           \
  }

#define MT_KICKOFF_WRITE_READ_THR(writers_, mswap_thr_, src_dst_, stats_, params_, write_func_, read_func_)            \
  pthread_t mswap_thr_[max_mswap_thr];                                                                                 \
  FOREACH_MSWAP_THREAD(thr, src_dst_, stats_, params) {                                                                \
    void *(*thr_func)(void *) = write_func_;                                                                           \
    if (thr >= writers_)                                                                                               \
      thr_func = read_func_;                                                                                           \
    int creation_ok = pthread_create(mswap_thr_ + thr, NULL, thr_func, PASTIFY(params_, _i));                          \
    TEST_ASSERT(creation_ok == 0, "Failed to create reader thread %d", thr);                                           \
  }

#define MT_JOIN_MSWAP_THREADS(mswap_thr_, mswap_dst_, stats_, params_)                                                 \
  {                                                                                                                    \
    int join_ok = join_all(mswap_thr_, max_mswap_thr, mswap_rounds + 666);                                             \
    TEST_ASSERT(join_ok == 0, "Reader threads never completed");                                                       \
  }                                                                                                                    \
  print_stats_from_threads(stats_, max_mswap_thr);                                                                     \
  uint32_t acceptable_success = (uint32_t)(mswap_rounds * 0.99) + 1;                                                   \
  FOREACH_MSWAP_THREAD(thr, mswap_dst_, stats_, params_) {                                                             \
    TEST_ASSERT(PASTIFY(stats_, _i)->success >= acceptable_success, "Thread %d failed to read", thr);                  \
  }

#define MT_ASSERT_CONTENTION(mswap_dst_, stats_, params_)                                                              \
  {                                                                                                                    \
    uint32_t acquire_tries = 0, release_tries = 0;                                                                     \
    FOREACH_MSWAP_THREAD(thr, mswap_dst_, stats_, params) {                                                            \
      acquire_tries += stats_##_i->acquire_tries;                                                                      \
      release_tries += stats_##_i->release_tries;                                                                      \
    }                                                                                                                  \
    TEST_ASSERT(acquire_tries > 0 && release_tries > 0, "No contention %u / %u", acquire_tries, release_tries);        \
  }

#define FOREACH_MSWAP_THREAD(thr_, mswap_dst_, stats_, params_)                                                        \
  for (uint32_t thr_ = 0, __ir = 0; thr_ < max_mswap_thr; ++thr_, __ir = thr_)                                         \
    for (TestContent *mswap_dst_##_i = mswap_dst_ + thr_; mswap_dst_##_i && __ir == thr_;)                             \
      for (MSWAP_Stat *stats_##_i = stats_ + thr_; stats_##_i && __ir == thr_;)                                        \
        for (HelperParams *params_##_i = params_ + thr_; params_##_i && __ir == thr_; ++__ir)


struct TestContent {
  uint64_t x, y, z;
};

struct HelperParams {
  MSWAP_Count *mswap;
  struct TestContent *target;
  MSWAP_Stat *stats;
  uint32_t thr_iterations;
  uint32_t wait_for;
  uint32_t cur_slot, to_slot;
  pthread_mutex_t stop;
};

struct HelpLockParams {
  Swap_Mutex mutex;
  Swap_Spin spin;
  Swap_Rwlock rwlock;
  struct TestContent *target;
  uint32_t thr_iterations;
  uint32_t wait_for;
};

typedef struct TestContent TestContent;
typedef struct HelperParams HelperParams;
typedef struct HelpLockParams HelpLockParams;
typedef TestContent TestContentSlots[SWCOUNT_SLOTS];

void test_content_reader_func(void *restrict src, uint32_t slot, void *restrict dst);
void test_content_writer_func(void *restrict src, uint32_t slot, void *restrict dst);
void test_lock_reader_func(void *restrict src, void *restrict dst);
void test_lock_writer_func(void *restrict src, void *restrict dst);
void print_stats_from_threads(MSWAP_Stat *stats, uint32_t thr_count);

//////////////////////////////////////////////////////////////////////////////////////////////

void test_count_mswap_parameters();
void test_count_mswap_alignment();
void test_bit_field_mem_layout();
void test_bit_field_overflow();
void main_common_unit_test();

//////////////////////////////////////////////////////////////////////////////////////////////
