#include <util.h>

#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <common.h>
#include <logger.h>

uint32_t busy_spin_for(uint32_t iterations) {
  volatile uint32_t i = 0;
  if(!iterations) return 0;
  iterations = ((uint32_t)random() % iterations) + iterations;
  while (i++ < iterations)
    ;
  return i;
}

int join_all(void *threads_raw, uint32_t thread_count, uint32_t millis_timeout) {
  pthread_t *threads = (pthread_t *)threads_raw;
  struct timespec timeout = {.tv_sec = millis_timeout / 1000, .tv_nsec = (millis_timeout % 1000) * 1000000 };
  struct timespec abstime;

  for (uint32_t idx = 0; idx < thread_count; ++idx) {
    if (clock_gettime(CLOCK_REALTIME_COARSE, &abstime))
      return -1;
    abstime.tv_sec += timeout.tv_sec + (abstime.tv_nsec + timeout.tv_nsec) / 1000000000;
    abstime.tv_nsec = (abstime.tv_nsec + timeout.tv_nsec) % 1000000000;

#ifdef __SANITIZER_THREAD_ON__
    int result = pthread_join(threads[idx], NULL);
#else
    // the santizer does not recognize pthread_timedjoin_np so it reports a leak
    int result = pthread_timedjoin_np(threads[idx], NULL, &abstime);
#endif
    if (result) {
      LOG_WARN("Failed to join %p = %d after %u", threads + idx, result, millis_timeout);
      return result;
    }
  }
  return 0;
}

int print_mem(void *start, uint32_t bytes, char* buffer) {
  for(uint32_t i=0; i<bytes; ++i) {
    snprintf(buffer + 3*i, 4, "%02X ", *((uint8_t*)start+i));
  }
  return bytes * 3;
}
