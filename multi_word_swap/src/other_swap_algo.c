#include <other_swap_algo.h>

#include <logger.h>

#include <stdlib.h>
#include <pthread.h>
IGNORE_WARNING_PUSH("-Wunused-variable")

//////////////////////////////////////////////////////////////////////////////////////////////

Swap_Mutex mutex_mswap_init(void *content) {
  Swap_Mutex swap = {.content=content};
  pthread_mutexattr_t attr;
  swap.mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutexattr_init(&attr);
  pthread_mutex_init(swap.mutex, &attr);
  return swap;
}

void mutex_mswap_clean(Swap_Mutex swap) {
  int failed = pthread_mutex_trylock(swap.mutex);
  ASSERT(!failed, "Cannot destroy locked mutex");
  pthread_mutex_unlock(swap.mutex);
  pthread_mutex_destroy(swap.mutex);
  free((void*)swap.mutex);
}

int mutex_mswap_read(Swap_Mutex swap, void *restrict read_dst, lock_content_reader read_func) {
  int failed = pthread_mutex_lock(swap.mutex);
  ASSERT(!failed, "failed to acquire mutex");
  read_func(swap.content, read_dst);
  failed = pthread_mutex_unlock(swap.mutex);
  ASSERT(!failed, "failed to release mutex");
  return !failed;
}

int mutex_mswap_write(Swap_Mutex mutex, void *restrict write_src, lock_content_writer write_func) {
  return mutex_mswap_read(mutex, write_src, write_func);
}

//////////////////////////////////////////////////////////////////////////////////////////////

Swap_Spin spin_mswap_init(void *content) {
  Swap_Spin spin = {.content=content};
  spin.mutex = malloc(sizeof(pthread_spinlock_t));
  pthread_spin_init(spin.mutex, PTHREAD_PROCESS_PRIVATE);
  return spin;
}

void spin_mswap_clean(Swap_Spin spin) {
  int failed = pthread_spin_trylock(spin.mutex);
  ASSERT(!failed, "cannot destroy a locked spin lock");
  pthread_spin_unlock(spin.mutex);
  pthread_spin_destroy(spin.mutex);
  free((void*)spin.mutex);
}

int spin_mswap_read(Swap_Spin spin, void *restrict read_dst, lock_content_reader read_func) {
  int failed = pthread_spin_lock(spin.mutex);
  ASSERT(!failed, "Could not lock spin lock");
  read_func(spin.content, read_dst);
  failed = pthread_spin_unlock(spin.mutex);
  ASSERT(!failed, "Could not unlock spin lock");
  return !failed;
}

int spin_mswap_write(Swap_Spin spin, void *restrict write_src, lock_content_writer write_func) {
  return spin_mswap_read(spin, write_src, write_func);
}

//////////////////////////////////////////////////////////////////////////////////////////////

Swap_Rwlock rwlock_mswap_init(void *content) {
  Swap_Rwlock swap = {.content=content};
  pthread_rwlockattr_t attr;
  swap.mutex = malloc(sizeof(pthread_rwlock_t));
  pthread_rwlockattr_init(&attr);
  pthread_rwlockattr_setkind_np(&attr, PTHREAD_RWLOCK_PREFER_READER_NP);
  pthread_rwlock_init(swap.mutex, &attr);
  return swap;
}

void rwlock_mswap_clean(Swap_Rwlock swap) {
  int failed = pthread_rwlock_trywrlock(swap.mutex);
  ASSERT(!failed, "cannot destroy lock with readers or writers locked");
  pthread_rwlock_unlock(swap.mutex);
  pthread_rwlock_destroy(swap.mutex);
  free((void*)swap.mutex);
}

int rwlock_mswap_read(Swap_Rwlock swap, void *restrict read_dst, lock_content_reader read_func) {
  int failed = pthread_rwlock_rdlock(swap.mutex);
  ASSERT(!failed, "could not lock for reading");
  read_func(swap.content, read_dst);
  failed = pthread_rwlock_unlock(swap.mutex);
  ASSERT(!failed, "could not unlock for reading");
  return !failed;
}

int rwlock_mswap_write(Swap_Rwlock swap, void *restrict write_src, lock_content_writer write_func) {
  int failed = pthread_rwlock_wrlock(swap.mutex);
  ASSERT(!failed, "could not lock for writing");
  write_func(swap.content, write_src);
  failed = pthread_rwlock_unlock(swap.mutex);
  ASSERT(!failed, "could not unlock for writing");
  return !failed;
}

//////////////////////////////////////////////////////////////////////////////////////////////
