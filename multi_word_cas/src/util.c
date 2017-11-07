#include <util.h>

#include <pthread.h>
#include <string.h>
#include <time.h>

#include <logger.h>

uint32_t busy_wait_helper(volatile uintmax_t *condition, uintmax_t init_val, uint32_t iterations) {
  uint32_t i = 0;
  while (*condition == init_val && i++ < iterations)
    ;
  return i;
}

uint32_t busy_read_wait(uintmax_t *condition, uintmax_t init_val) {
  return busy_wait_helper(condition, init_val, WAIT_READ_LOCK_FULL);
}

uint32_t busy_write_wait(uintmax_t *condition, uintmax_t init_val) {
  return busy_wait_helper(condition, init_val, WAIT_WRITE_LOCK_FULL);
}

void reset_stats(MCAS_Stat *stats) { memset(stats, 0, sizeof(MCAS_Stat)); }

int join_all(void *threads_raw, uint32_t thread_count, uint32_t millis_timeout) {
  pthread_t *threads = (pthread_t *)threads_raw;
  struct timespec timeout = {.tv_sec = millis_timeout / 1000, .tv_nsec = (millis_timeout % 1000) * 1000000 };
  struct timespec abstime;

  for (uint32_t idx = 0; idx < thread_count; ++idx) {
    if (clock_gettime(CLOCK_REALTIME_COARSE, &abstime))
      return -1;
    abstime.tv_sec += timeout.tv_sec + (abstime.tv_nsec + timeout.tv_nsec) / 1000000000;
    abstime.tv_nsec = (abstime.tv_nsec + timeout.tv_nsec) % 1000000000;

    int result = pthread_timedjoin_np(threads[idx], NULL, &abstime);
    //int result = pthread_join(threads[idx], NULL);
    if (result) {
      LOG_WARN("Failed to join %p = %d after %u", threads + idx, result, millis_timeout);
      return result;
    }
  }
  return 0;
}
