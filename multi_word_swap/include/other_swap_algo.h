#pragma once

#include <common.h>
#include <multi_swap_ctrl.h>

#include <stdint.h>
#include <pthread.h>

//////////////////////////////////////////////////////////////////////////////////////////////

typedef void (*lock_content_writer)(void *restrict, void *restrict);
typedef void (*lock_content_reader)(void *restrict, void *restrict);

//////////////////////////////////////////////////////////////////////////////////////////////

typedef struct Swap_Mutex {
  pthread_mutex_t *mutex;
  void *content;
} Swap_Mutex;

Swap_Mutex mutex_swap_init(void *);
void mutex_swap_clean(Swap_Mutex);
int mutex_swap_read(Swap_Mutex, void *restrict, lock_content_reader);
int mutex_swap_write(Swap_Mutex, void *restrict, lock_content_writer);

//////////////////////////////////////////////////////////////////////////////////////////////

typedef struct Swap_Spin {
  pthread_spinlock_t *mutex;
  void *content;
} Swap_Spin;

Swap_Spin spin_swap_init(void *);
void spin_swap_clean(Swap_Spin);
int spin_swap_read(Swap_Spin, void *restrict, lock_content_reader);
int spin_swap_write(Swap_Spin, void *restrict, lock_content_writer);

//////////////////////////////////////////////////////////////////////////////////////////////

typedef struct Swap_Rwlock {
  pthread_rwlock_t *mutex;
  void *content;
} Swap_Rwlock;

Swap_Rwlock rwlock_swap_init(void *);
void rwlock_swap_clean(Swap_Rwlock);
int rwlock_swap_read(Swap_Rwlock, void *restrict, lock_content_reader);
int rwlock_swap_write(Swap_Rwlock, void *restrict, lock_content_writer);

//////////////////////////////////////////////////////////////////////////////////////////////
