#pragma once
#include "multi_cas_ctrl.h"

#include <stdint.h>

#define WAIT_READ_LOCK_FULL 100
#define WAIT_WRITE_LOCK_FULL 100


uint32_t busy_write_wait(uintmax_t *, uintmax_t);
uint32_t busy_read_wait(uintmax_t *, uintmax_t);

void reset_stats(MCAS_Stat *stats);
int join_all(void *threads, uint32_t thread_count, uint32_t millis_timeout);
