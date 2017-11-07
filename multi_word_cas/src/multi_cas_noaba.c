#include <common.h>
#include <logger.h>
#include <multi_cas_noaba.h>
#include <util.h>

#include <stdatomic.h>
#include <stdbool.h>
#include <stdlib.h>

//////////////////////////////////////////////////////////////////////////////////////////////

MCas_NoABA build_mcas_noaba(void *content) {
  MCas_NoABA mcas = { 0 };
  mcas.content = content;
  return mcas;
}

IGNORE_WARNING_PUSH("-Wunused-variable")
void free_mcas_noaba(MCas_NoABA *mcas) {
  uintmax_t raw_ctrl = atomic_load_explicit(&(mcas->control.raw), memory_order_relaxed);
  ASSERT((version_lock_mask() & raw_ctrl) == 0 && (writer_slot_mask() & raw_ctrl) == 0,
         "Cannot delete while there are still readers or writers");

  if (mcas->content) {
    free(mcas->content);
    mcas->content = NULL;
  }
}
IGNORE_WARNING_POP

//////////////////////////////////////////////////////////////////////////////////////////////

MCAS_Stat noaba_cas_read(MCas_NoABA *mcas, void *read_dest, cas_content_reader reader) {
  MCAS_Stat stats = { 0 };
  LockResult lock_result = lock_for_reading(mcas, &stats);
  stats.success = lock_result.success;

  if (lock_result.success) {
    CtrlView_NoABA lock_view = lock_result.view;
    reader(mcas->content, lock_view.fields.latest_slot, read_dest);
    stats.success = release_read_lock(mcas, lock_view, &stats);
  }
  return stats;
}

MCAS_Stat noaba_cas_write(MCas_NoABA *control, void *new_value, cas_content_writer writer) {
  MCAS_Stat stats = { 0 };
  return stats;
}

//////////////////////////////////////////////////////////////////////////////////////////////

LockResult lock_for_reading(MCas_NoABA *mcas, MCAS_Stat *stats) {
  __auto_type ctrl_ptr = &(mcas->control.raw);
  CtrlView_NoABA src_view, dst_view;
  src_view.raw = atomic_load_explicit(ctrl_ptr, memory_order_relaxed);

  CAS_LOOP_OR_FAIL {
    dst_view.raw = src_view.raw;
    bool is_locked = increment_latest_reader_lock(&dst_view, stats);
    bool is_swapped
        = is_locked && atomic_compare_exchange_weak_explicit(ctrl_ptr, (uintmax_t *)&src_view.raw, dst_view.raw,
                                                             memory_order_release, memory_order_relaxed);

    if (is_swapped)
      break;
    if (!is_locked) {
      uint32_t wait_cycles = busy_read_wait((uintmax_t *)ctrl_ptr, src_view.raw);
      stats->wait_spins += wait_cycles;
    }
    stats->acquire_tries += 1;
  }

  LockResult result = {.view = dst_view, .success = IS_CAS_OK };
  return result;
}

uint32_t increment_latest_reader_lock(CtrlView_NoABA *view, MCAS_Stat *stats) {
  int free_version_idx = -1;

  FOREACH_VERSION_LOCK(*view, desc) {
    ASSERT(desc.lock < NOABA_READERS, "Lock is overflowing");

    if (desc.slot == view->fields.latest_slot) {
      if (desc.lock == NOABA_READERS - 1) {
        stats->version_overflow += 1;
        return false;
      }
      desc.lock += 1;
      set_version_lock(view, desc);
      return true;
    }
    if (desc.lock == 0)
      free_version_idx = desc.version;
  }

  if (free_version_idx >= 0) {
    LockDesc new_lock = {.slot = view->fields.latest_slot, .lock = 1, .version = free_version_idx };
    set_version_lock(view, new_lock);
    return true;
  }

  stats->all_ver_taken += 1;
  return false;
}

bool release_read_lock(MCas_NoABA *mcas, CtrlView_NoABA src_view, MCAS_Stat *stats) {
  CtrlView_NoABA dst_view;
  LockDesc desc = find_lock_desc_by_slot(src_view, src_view.fields.latest_slot);
  ASSERT(IS_LOCK_DESC_VALID(desc), "Did not find the version lock for latest_slot");

  CAS_LOOP_OR_FAIL {
    ASSERT(desc.lock > 0, "Lock was released more than once");
    dst_view.raw = src_view.raw;
    desc.lock -= 1;
    set_version_lock(&dst_view, desc);

    bool is_swapped = atomic_compare_exchange_weak_explicit(&(mcas->control.raw), (uintmax_t *)&src_view.raw,
                                                            dst_view.raw, memory_order_release, memory_order_relaxed);
    if (is_swapped)
      break;

    desc = lock_descriptor_at(src_view, desc.version);
    stats->release_tries += 1;
  }
  return IS_CAS_OK;
}

//////////////////////////////////////////////////////////////////////////////////////////////

uintmax_t version_lock_mask() {
  uintmax_t mask = 0;
  uintmax_t floating_mask = ((1 << NOABA_READER_EXP) - 1) << (NOABA_SLOT_EXP + NOABA_SLOTS);
  for (uint32_t i = 0; i < NOABA_VERSION_COUNT; ++i) {
    mask |= floating_mask;
    floating_mask = floating_mask << (NOABA_READER_EXP + NOABA_SLOT_EXP);
  }
  return mask;
}

uintmax_t writer_slot_mask() {
  uintmax_t mask = 0;
  mask = ((1 << NOABA_SLOTS) - 1) << NOABA_SLOT_EXP;
  return mask;
}

LockDesc lock_descriptor_at(CtrlView_NoABA view, uint32_t version_idx) {
  LockDesc desc = { 0 };
  uintmax_t mask_lock = (1 << NOABA_READER_EXP) - 1;
  uintmax_t mask_slot = (1 << NOABA_SLOT_EXP) - 1;

  desc.lock = view.fields.version_locks >> (version_idx * (NOABA_READER_EXP + NOABA_SLOT_EXP));
  desc.lock &= mask_lock;
  desc.slot = view.fields.version_locks >> (version_idx * (NOABA_READER_EXP + NOABA_SLOT_EXP));
  desc.slot = (desc.slot >> NOABA_READER_EXP) & mask_slot;
  desc.version = version_idx;
  return desc;
}

void set_version_lock(CtrlView_NoABA *dst_view, LockDesc desc) {
  uintmax_t negative = (1 << (NOABA_SLOT_EXP + NOABA_READER_EXP)) - 1;
  negative = negative << (desc.version * (NOABA_READER_EXP + NOABA_SLOT_EXP));
  negative = ~negative;
  uintmax_t mask = desc.lock + (desc.slot << NOABA_READER_EXP);
  mask = mask << (desc.version * (NOABA_READER_EXP + NOABA_SLOT_EXP));

  dst_view->fields.version_locks &= negative;
  dst_view->fields.version_locks |= mask;
}

LockDesc find_lock_desc_by_slot(CtrlView_NoABA view, uint32_t slot_id) {
  LockDesc desc = lock_descriptor_at(view, 0);
  for (; desc.version < NOABA_VERSION_COUNT; ++desc.version, desc = lock_descriptor_at(view, desc.version)) {
    if (desc.slot == slot_id)
      break;
  }
  return desc;
}
