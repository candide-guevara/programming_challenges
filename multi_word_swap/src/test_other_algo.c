#include <test_other_algo.h>

#include <common.h>
#include <logger.h>
#include <other_swap_algo.h>
#include <util.h>

#include <stdlib.h>
#include <sys/resource.h>

//////////////////////////////////////////////////////////////////////////////////////////////

#define THREAD_FUNCTION_PATTERN(lock_func, lock_name, rw_func)                                                         \
  static void *lock_func##_in_loop(void *params_raw) {                                                                 \
    HelpLockParams *params = (HelpLockParams *)params_raw;                                                             \
    while (params->thr_iterations--) {                                                                                 \
      lock_func(params->lock_name, params->target, rw_func);                                                           \
      if (params->wait_for)                                                                                            \
        busy_spin_for(params->wait_for);                                                                               \
    }                                                                                                                  \
    return params_raw;                                                                                                 \
  }

#define type_swap_mutex Swap_Mutex
#define type_swap_spin Swap_Spin
#define type_swap_rwlock Swap_Rwlock

#define PX_MSWAP_BOILER_PLATE(max_mswap_thr_, thr_iterations_, field_name, mswap_, mswap_dst_, params_)                \
  const uint32_t max_mswap_thr = max_mswap_thr_;                                                                       \
  const uint32_t mswap_rounds = thr_iterations_;                                                                       \
  TestContent content_ = { 3, 3, 3 };                                                                                  \
  TestContent mswap_dst_[max_mswap_thr_] = {[0 ... max_mswap_thr_ - 1] = {.x = 111, .y = 222, .z = 333 } };            \
  HelpLockParams params_[max_mswap_thr_] = { {} };                                                                     \
  type_swap_##field_name mswap_ = field_name##_mswap_init(&content_);                                                   \
  for (uint32_t i = 0; i < max_mswap_thr_; ++i) {                                                                      \
    params_[i].field_name = mswap_;                                                                                    \
    params_[i].target = mswap_dst_ + i;                                                                                \
    params_[i].thr_iterations = mswap_rounds;                                                                          \
    params_[i].wait_for = 0;                                                                                           \
  }

#define PX_JOIN_MSWAP_THREADS(mswap_thr_)                                                                              \
  {                                                                                                                    \
    int join_ok = join_all(mswap_thr_, max_mswap_thr, mswap_rounds + 666);                                             \
    TEST_ASSERT(join_ok == 0, "Reader threads never completed");                                                       \
  }

#define PX_KICKOFF_WRITE_READ_THR(writers_, field_name, mswap_thr_, src_dst_, params_)                                 \
  pthread_t mswap_thr_[max_mswap_thr];                                                                                 \
  PX_FOREACH_MSWAP_THREAD(thr, src_dst_, params) {                                                                     \
    void *(*thr_func)(void *) = field_name##_mswap_write_in_loop;                                                       \
    if (thr >= writers_)                                                                                               \
      thr_func = field_name##_mswap_read_in_loop;                                                                       \
    int creation_ok = pthread_create(mswap_thr_ + thr, NULL, thr_func, PASTIFY(params_, _i));                          \
    TEST_ASSERT(creation_ok == 0, "Failed to create reader thread %d", thr);                                           \
  }

#define PX_FOREACH_MSWAP_THREAD(thr_, mswap_dst_, params_)                                                             \
  for (uint32_t thr_ = 0, __ir = 0; thr_ < max_mswap_thr; ++thr_, __ir = thr_)                                         \
    for (TestContent *mswap_dst_##_i = mswap_dst_ + thr_; mswap_dst_##_i && __ir == thr_;)                             \
      for (HelpLockParams *params_##_i = params_ + thr_; params_##_i && __ir == thr_; ++__ir)

THREAD_FUNCTION_PATTERN(mutex_mswap_read, mutex, test_lock_reader_func)
THREAD_FUNCTION_PATTERN(mutex_mswap_write, mutex, test_lock_writer_func)
THREAD_FUNCTION_PATTERN(spin_mswap_read, spin, test_lock_reader_func)
THREAD_FUNCTION_PATTERN(spin_mswap_write, spin, test_lock_writer_func)
THREAD_FUNCTION_PATTERN(rwlock_mswap_read, rwlock, test_lock_reader_func)
THREAD_FUNCTION_PATTERN(rwlock_mswap_write, rwlock, test_lock_writer_func)

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_mutex_mswap_read_single)
  PX_MSWAP_BOILER_PLATE(1, 1000, mutex, swap, read_dst, params);
  PX_KICKOFF_WRITE_READ_THR(0, mutex, swap_thr, read_dst, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  PX_FOREACH_MSWAP_THREAD(thr, read_dst, params) {
    TEST_ASSERT(read_dst_i->x == 3 && read_dst_i->y == 3 && read_dst_i->z == 3, "Bad read");
  }
  mutex_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_mutex_mswap_write_single)
  PX_MSWAP_BOILER_PLATE(1, 1000, mutex, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(1, mutex, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  PX_FOREACH_MSWAP_THREAD(thr, write_src, params) {
    TestContent *content = swap.content;
    TEST_ASSERT(content->x == 111 && content->y == 222 && content->z == 333, "Bad write");
  }
  mutex_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_mutex_mswap_mt_read_unblock)
  PX_MSWAP_BOILER_PLATE(1, 1, mutex, swap, read_dst, params);
  pthread_mutex_lock(params->mutex.mutex);
  PX_KICKOFF_WRITE_READ_THR(0, mutex, swap_thr, read_dst, params);
  busy_spin_for(1024);
  PX_FOREACH_MSWAP_THREAD(thr, read_dst, params) {
    TEST_ASSERT(read_dst_i->x != 3 && read_dst_i->y != 3 && read_dst_i->z != 3, "Read without lock");
  }
  pthread_mutex_unlock(params->mutex.mutex);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  mutex_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_mutex_mswap_mt_write_unblock)
  PX_MSWAP_BOILER_PLATE(1, 1, mutex, swap, write_src, params);
  pthread_mutex_lock(params->mutex.mutex);
  PX_KICKOFF_WRITE_READ_THR(1, mutex, swap_thr, write_src, params);
  busy_spin_for(1024);
  PX_FOREACH_MSWAP_THREAD(thr, write_src, params) {
    TestContent *content = swap.content;
    TEST_ASSERT(content->x != 111 && content->y != 222 && content->z != 333, "Write without lock");
  }
  pthread_mutex_unlock(params->mutex.mutex);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  mutex_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_mutex_mswap_mt_many_readers)
  PX_MSWAP_BOILER_PLATE(6, 10000, mutex, swap, read_dst, params);
  PX_KICKOFF_WRITE_READ_THR(0, mutex, swap_thr, read_dst, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  mutex_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_mutex_mswap_mt_many_writers)
  PX_MSWAP_BOILER_PLATE(6, 10000, mutex, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(6, mutex, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  mutex_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_mutex_mswap_mt_many_readers_writers)
  PX_MSWAP_BOILER_PLATE(6, 10000, mutex, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(3, mutex, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  mutex_mswap_clean(swap);
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_spin_mswap_read_single)
  PX_MSWAP_BOILER_PLATE(1, 1000, spin, swap, read_dst, params);
  PX_KICKOFF_WRITE_READ_THR(0, spin, swap_thr, read_dst, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  PX_FOREACH_MSWAP_THREAD(thr, read_dst, params) {
    TEST_ASSERT(read_dst_i->x == 3 && read_dst_i->y == 3 && read_dst_i->z == 3, "Bad read");
  }
  spin_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_spin_mswap_write_single)
  PX_MSWAP_BOILER_PLATE(1, 1000, spin, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(1, spin, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  PX_FOREACH_MSWAP_THREAD(thr, write_src, params) {
    TestContent *content = swap.content;
    TEST_ASSERT(content->x == 111 && content->y == 222 && content->z == 333, "Bad write");
  }
  spin_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_spin_mswap_mt_read_unblock)
  PX_MSWAP_BOILER_PLATE(1, 1, spin, swap, read_dst, params);
  pthread_spin_lock(params->spin.mutex);
  PX_KICKOFF_WRITE_READ_THR(0, spin, swap_thr, read_dst, params);
  busy_spin_for(1024);
  PX_FOREACH_MSWAP_THREAD(thr, read_dst, params) {
    TEST_ASSERT(read_dst_i->x != 3 && read_dst_i->y != 3 && read_dst_i->z != 3, "Read without lock");
  }
  pthread_spin_unlock(params->spin.mutex);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  spin_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_spin_mswap_mt_write_unblock)
  PX_MSWAP_BOILER_PLATE(1, 1, spin, swap, write_src, params);
  pthread_spin_lock(params->spin.mutex);
  PX_KICKOFF_WRITE_READ_THR(1, spin, swap_thr, write_src, params);
  busy_spin_for(1024);
  PX_FOREACH_MSWAP_THREAD(thr, write_src, params) {
    TestContent *content = swap.content;
    TEST_ASSERT(content->x != 111 && content->y != 222 && content->z != 333, "Write without lock");
  }
  pthread_spin_unlock(params->spin.mutex);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  spin_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_spin_mswap_mt_many_readers)
  PX_MSWAP_BOILER_PLATE(6, 10000, spin, swap, read_dst, params);
  PX_KICKOFF_WRITE_READ_THR(0, spin, swap_thr, read_dst, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  spin_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_spin_mswap_mt_many_writers)
  PX_MSWAP_BOILER_PLATE(6, 10000, spin, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(6, spin, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  spin_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_spin_mswap_mt_many_readers_writers)
  PX_MSWAP_BOILER_PLATE(6, 10000, spin, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(3, spin, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  spin_mswap_clean(swap);
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_rwlock_mswap_read_single)
  PX_MSWAP_BOILER_PLATE(1, 1000, rwlock, swap, read_dst, params);
  PX_KICKOFF_WRITE_READ_THR(0, rwlock, swap_thr, read_dst, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  PX_FOREACH_MSWAP_THREAD(thr, read_dst, params) {
    TEST_ASSERT(read_dst_i->x == 3 && read_dst_i->y == 3 && read_dst_i->z == 3, "Bad read");
  }
  rwlock_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_rwlock_mswap_write_single)
  PX_MSWAP_BOILER_PLATE(1, 1000, rwlock, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(1, rwlock, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  PX_FOREACH_MSWAP_THREAD(thr, write_src, params) {
    TestContent *content = swap.content;
    TEST_ASSERT(content->x == 111 && content->y == 222 && content->z == 333, "Bad write");
  }
  rwlock_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_rwlock_mswap_mt_read_unblock)
  PX_MSWAP_BOILER_PLATE(1, 1, rwlock, swap, read_dst, params);
  pthread_rwlock_wrlock(params->rwlock.mutex);
  PX_KICKOFF_WRITE_READ_THR(0, rwlock, swap_thr, read_dst, params);
  busy_spin_for(1024);
  PX_FOREACH_MSWAP_THREAD(thr, read_dst, params) {
    TEST_ASSERT(read_dst_i->x != 3 && read_dst_i->y != 3 && read_dst_i->z != 3, "Read without lock");
  }
  pthread_rwlock_unlock(params->rwlock.mutex);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  rwlock_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_rwlock_mswap_mt_write_unblock)
  PX_MSWAP_BOILER_PLATE(1, 1, rwlock, swap, write_src, params);
  pthread_rwlock_rdlock(params->rwlock.mutex);
  PX_KICKOFF_WRITE_READ_THR(1, rwlock, swap_thr, write_src, params);
  busy_spin_for(1024);
  PX_FOREACH_MSWAP_THREAD(thr, write_src, params) {
    TestContent *content = swap.content;
    TEST_ASSERT(content->x != 111 && content->y != 222 && content->z != 333, "Write without lock");
  }
  pthread_rwlock_unlock(params->rwlock.mutex);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  rwlock_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_rwlock_mswap_mt_many_readers)
  PX_MSWAP_BOILER_PLATE(6, 10000, rwlock, swap, read_dst, params);
  PX_KICKOFF_WRITE_READ_THR(0, rwlock, swap_thr, read_dst, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  rwlock_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_rwlock_mswap_mt_many_writers)
  PX_MSWAP_BOILER_PLATE(6, 10000, rwlock, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(6, rwlock, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  rwlock_mswap_clean(swap);
UNIT_TEST_END

UNIT_TEST_START(test_rwlock_mswap_mt_many_readers_writers)
  PX_MSWAP_BOILER_PLATE(6, 10000, rwlock, swap, write_src, params);
  PX_KICKOFF_WRITE_READ_THR(3, rwlock, swap_thr, write_src, params);
  PX_JOIN_MSWAP_THREADS(swap_thr);
  rwlock_mswap_clean(swap);
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////
