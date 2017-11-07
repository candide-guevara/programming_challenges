#pragma once

#include "multi_cas_ctrl.h"

#define MAX_CAS_ATTEMPTS 10
#define CAS_LOOP_OR_FAIL                                                                                               \
  uint32_t __cas_count__ = 0;                                                                                          \
  for (__cas_count__ = 0; __cas_count__ < MAX_CAS_ATTEMPTS; ++__cas_count__)
#define IS_CAS_OK (__cas_count__ < MAX_CAS_ATTEMPTS)

#define FOREACH_VERSION_LOCK(view, desc) __FOREACH_VERSION_LOCK__(view, desc, __COUNTER__)
#define __FOREACH_VERSION_LOCK__(view, desc, counter)                                                                  \
  for (LockDesc desc = lock_descriptor_at(view, 0); desc.version < NOABA_VERSION_COUNT;                                \
       ++desc.version, desc = lock_descriptor_at(view, desc.version))

#define IS_LOCK_DESC_VALID(desc) (desc.version < NOABA_VERSION_COUNT)

//////////////////////////////////////////////////////////////////////////////////////////////

MCAS_Stat noaba_cas_read(MCas_NoABA *, void *, cas_content_reader);
MCAS_Stat noaba_cas_write(MCas_NoABA *, void *, cas_content_writer);

//////////////////////////////////////////////////////////////////////////////////////////////

MCas_NoABA build_mcas_noaba(void *);
void free_mcas_noaba(MCas_NoABA *);

//////////////////////////////////////////////////////////////////////////////////////////////

uintmax_t version_lock_mask();
uintmax_t writer_slot_mask();
LockDesc lock_descriptor_at(CtrlView_NoABA, uint32_t);
LockDesc find_lock_desc_by_slot(CtrlView_NoABA, uint32_t);
void set_version_lock(CtrlView_NoABA *, LockDesc);

typedef struct LockResult {
  CtrlView_NoABA view;
  bool success;
} LockResult;
LockResult lock_for_reading(MCas_NoABA *, MCAS_Stat *);

uint32_t increment_latest_reader_lock(CtrlView_NoABA *, MCAS_Stat *);
bool release_read_lock(MCas_NoABA *, CtrlView_NoABA, MCAS_Stat *);
