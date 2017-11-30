#pragma once

#include "multi_swap_ctrl.h"

#define CAS_LOOP_FOR(max_attempts, is_swap_ok)                                                                          \
  uint32_t is_swap_ok = 0 < max_attempts;                                                                               \
  for (uint32_t __swap_count__ = 0; is_swap_ok; ++__swap_count__, is_swap_ok = __swap_count__ < max_attempts)

#define FOREACH_VERSION_LOCK(view, desc)                                                                               \
  for (LockDesc desc = lock_descriptor_at(view, 0); desc.version < SWCOUNT_VERSION_COUNT;                                \
       ++desc.version, desc = lock_descriptor_at(view, desc.version))

#define IS_LOCK_DESC_VALID(desc) (desc.version < SWCOUNT_VERSION_COUNT)

//////////////////////////////////////////////////////////////////////////////////////////////

MSWAP_Stat count_mswap_read(MSWAP_Count *, void *restrict, swap_content_reader);
MSWAP_Stat count_mswap_write(MSWAP_Count *, void *restrict, swap_content_writer);

//////////////////////////////////////////////////////////////////////////////////////////////

MSWAP_Count build_mswap_count(void *);
void free_mswap_count(MSWAP_Count *);
void reset_stats(MSWAP_Stat *stats);
void mswap_stats_merge(MSWAP_Stat *source, MSWAP_Stat *dest);
uint32_t mswap_stats_print(char *, MSWAP_Stat);

//////////////////////////////////////////////////////////////////////////////////////////////

uintmax_t version_lock_mask();
uintmax_t writer_slot_mask();
uintmax_t writer_mask_for_all_reading_slots(CtrlView_Count);
void purge_all_version_locks(CtrlView_Count *);
LockDesc lock_descriptor_at(CtrlView_Count, uint32_t);
void set_version_lock(CtrlView_Count *, LockDesc);
void evaluate_and_wait_under_contention(MSWAP_Stat *, int);

typedef struct LockResult {
  CtrlView_Count view;
  uint32_t position; // either version for read or slot for write
  uint32_t success;
} LockResult;
typedef LockResult (*try_lock_func_t)(CtrlView_Count, MSWAP_Stat *);
LockResult lock_for_read_or_write(MSWAP_Count *, MSWAP_Stat *, try_lock_func_t, int);

LockResult increment_latest_reader_lock(CtrlView_Count, MSWAP_Stat *);
LockResult set_writer_mask_for_booked_slot(CtrlView_Count, MSWAP_Stat *);
bool release_read_lock(MSWAP_Count *, CtrlView_Count, uint32_t, MSWAP_Stat *);
bool release_write_lock(MSWAP_Count *, CtrlView_Count, uint32_t, MSWAP_Stat *);
