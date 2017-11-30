#pragma once

#include "multi_cas_ctrl.h"

#define CAS_LOOP_FOR(max_attempts, is_cas_ok)                                                                          \
  uint32_t is_cas_ok = 0 < max_attempts;                                                                               \
  for (uint32_t __cas_count__ = 0; is_cas_ok; ++__cas_count__, is_cas_ok = __cas_count__ < max_attempts)

#define FOREACH_VERSION_LOCK(view, desc)                                                                               \
  for (LockDesc desc = lock_descriptor_at(view, 0); desc.version < NOABA_VERSION_COUNT;                                \
       ++desc.version, desc = lock_descriptor_at(view, desc.version))

#define IS_LOCK_DESC_VALID(desc) (desc.version < NOABA_VERSION_COUNT)

//////////////////////////////////////////////////////////////////////////////////////////////

MCAS_Stat noaba_cas_read(MCas_NoABA *, void *restrict, cas_content_reader);
MCAS_Stat noaba_cas_write(MCas_NoABA *, void *restrict, cas_content_writer);

//////////////////////////////////////////////////////////////////////////////////////////////

MCas_NoABA build_mcas_noaba(void *);
void free_mcas_noaba(MCas_NoABA *);
void reset_stats(MCAS_Stat *stats);
void mcas_stats_merge(MCAS_Stat *source, MCAS_Stat *dest);
uint32_t mcas_stats_print(char *, MCAS_Stat);

//////////////////////////////////////////////////////////////////////////////////////////////

uintmax_t version_lock_mask();
uintmax_t writer_slot_mask();
uintmax_t writer_mask_for_all_reading_slots(CtrlView_NoABA);
void purge_all_version_locks(CtrlView_NoABA *);
LockDesc lock_descriptor_at(CtrlView_NoABA, uint32_t);
void set_version_lock(CtrlView_NoABA *, LockDesc);
void evaluate_and_wait_under_contention(MCAS_Stat *, int);

typedef struct LockResult {
  CtrlView_NoABA view;
  uint32_t position; // eitjer version for read or slot for write
  uint32_t success;
} LockResult;
typedef LockResult (*try_lock_func_t)(CtrlView_NoABA, MCAS_Stat *);
LockResult lock_for_read_or_write(MCas_NoABA *, MCAS_Stat *, try_lock_func_t, int);

LockResult increment_latest_reader_lock(CtrlView_NoABA, MCAS_Stat *);
LockResult set_writer_mask_for_booked_slot(CtrlView_NoABA, MCAS_Stat *);
bool release_read_lock(MCas_NoABA *, CtrlView_NoABA, uint32_t, MCAS_Stat *);
bool release_write_lock(MCas_NoABA *, CtrlView_NoABA, uint32_t, MCAS_Stat *);
uint32_t emergency_version_lock_draining(MCas_NoABA *, CtrlView_NoABA, uint32_t);
