#include <multi_swap_count.h>

#include <common.h>
#include <logger.h>
#include <util.h>

#include <assert.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

//////////////////////////////////////////////////////////////////////////////////////////////

MSWAP_Stat count_mswap_read(MSWAP_Count *mswap, void *restrict read_dest, swap_content_reader reader) {
  MSWAP_Stat stats = { 0 };
  LockResult lock_result = lock_for_read_or_write(mswap, &stats, increment_latest_reader_lock, 1);
  stats.success = lock_result.success;

  if (lock_result.success) {
    CtrlView_Count lock_view = lock_result.view;
    reader(mswap->content, lock_view.latest_slot, read_dest);
    stats.success = release_read_lock(mswap, lock_view, lock_result.position, &stats);
  }
  return stats;
}

MSWAP_Stat count_mswap_write(MSWAP_Count *mswap, void *restrict new_value, swap_content_writer writer) {
  MSWAP_Stat stats = { 0 };
  LockResult lock_result = lock_for_read_or_write(mswap, &stats, set_writer_mask_for_booked_slot, 0);
  stats.success = lock_result.success;

  if (lock_result.success) {
    CtrlView_Count lock_view = lock_result.view;
    writer(mswap->content, lock_result.position, new_value);
    stats.success = release_write_lock(mswap, lock_view, lock_result.position, &stats);
  }
  return stats;
}

//////////////////////////////////////////////////////////////////////////////////////////////

MSWAP_Count build_mswap_count(void *content) {
  MSWAP_Count mswap = { 0 };
  mswap.content = content;
  return mswap;
}

IGNORE_WARNING_PUSH("-Wunused-variable")
void free_mswap_count(MSWAP_Count *mswap) {
  CtrlView_Count view = {._raw = atomic_load_explicit(&(mswap->control.raw), memory_order_relaxed) };
  ASSERT((version_lock_mask() & view._raw) == 0 && view.writer_mask == 0,
         "Cannot delete while there are still readers or writers");

  if (mswap->content) {
    free(mswap->content);
    mswap->content = NULL;
  }
}
IGNORE_WARNING_POP


void mswap_stats_merge(MSWAP_Stat *source, MSWAP_Stat *dest) {
  source->acquire_tries += dest->acquire_tries;
  source->release_tries += dest->release_tries;
  source->wait_spins += dest->wait_spins;
  source->version_overflow += dest->version_overflow;
  source->all_lock_taken += dest->all_lock_taken;
  source->success += dest->success;
}

uint32_t mswap_stats_print(char *buffer, MSWAP_Stat source) {
  return snprintf(buffer, 256, "[acquires=%d, releases=%d, overflows=%d, all_taken=%d, wait=%d, success=%d]",
                  source.acquire_tries, source.release_tries, source.version_overflow, source.all_lock_taken,
                  source.wait_spins, source.success);
}

void reset_stats(MSWAP_Stat *stats) { memset(stats, 0, sizeof(MSWAP_Stat)); }

//////////////////////////////////////////////////////////////////////////////////////////////

LockResult lock_for_read_or_write(MSWAP_Count *mswap, MSWAP_Stat *stats, try_lock_func_t attempt_lock_routine,
                                  int is_read) {
  __auto_type ctrl_ptr = &(mswap->control.raw);
#ifdef __SANITIZER_THREAD_ON__
  CtrlView_Count src_view;
#else
  CtrlView_Count src_view = mswap->control;
#endif
  LockResult book_ok = { 0 };

  CAS_LOOP_FOR(MAX_ACQUIRE_SWAP_ATTEMPTS, is_swap_ok) {
#if MORE_ATO_LESS_CONTENTION > 0 || defined __SANITIZER_THREAD_ON__
    src_view._raw = atomic_load_explicit(ctrl_ptr, memory_order_acquire);
#endif
    book_ok = attempt_lock_routine(src_view, stats);
    bool is_swapped
        = book_ok.success && atomic_compare_exchange_weak_explicit(ctrl_ptr, &src_view._raw, book_ok.view._raw,
                                                                   MEMORY_ORDER_ACQ_LOCK, memory_order_relaxed);

    if (is_swapped)
      break;
    stats->acquire_tries += 1;
    evaluate_and_wait_under_contention(stats, is_read);
  }

  book_ok.success &= is_swap_ok;
  return book_ok;
}

LockResult increment_latest_reader_lock(CtrlView_Count view, MSWAP_Stat *stats) {
  LockResult result = {.view = view, .success = 1, .position = -1 };

  FOREACH_VERSION_LOCK(view, desc) {
    ASSERT(desc.lock < SWCOUNT_READERS, "Lock is overflowing");

    if (desc.slot == view.latest_slot) {
      if (desc.lock == SWCOUNT_LAST_READER) {
        stats->version_overflow += 1;
        result.success = 0;
        return result;
      }
      desc.lock += 1;
      set_version_lock(&result.view, desc);
      result.position = desc.version;
      return result;
    }
    if (desc.lock == 0)
      result.position = desc.version;
  }

  if (result.position != -1) {
    LockDesc new_lock = {.slot = view.latest_slot, .lock = 1, .version = result.position };
    set_version_lock(&result.view, new_lock);
    return result;
  }

  stats->all_lock_taken += 1;
  result.success = 0;
  return result;
}

STATIC_ASSERT(SWCOUNT_SLOTS <= sizeof(uint32_t) * 8, cannot_handle_so_many_slots);
LockResult set_writer_mask_for_booked_slot(CtrlView_Count view, MSWAP_Stat *stats) {
  LockResult result = {.success = 1 };
  // We cannot overwrite that are being written or the latest written slot which is available for future readers
  uintmax_t writer_read_mask = writer_mask_for_all_reading_slots(view);

  writer_read_mask |= 1 << (uint8_t)(view.latest_slot);
  ASSERT((view.writer_mask & writer_read_mask) == 0, //
         "No used read or latest version : %lx, %lx", view._raw, writer_read_mask);
  writer_read_mask |= view.writer_mask;

  if (writer_read_mask == SWCOUNT_ALL_WRITERS) {
    stats->all_lock_taken += 1;
    result.success = 0;
    return result;
  }

  uint32_t shift_width = magical_get_high_pos_bit_set(~writer_read_mask & SWCOUNT_ALL_WRITERS);
  ASSERT(shift_width < SWCOUNT_SLOTS, "Bad shift width : %lx / %d", writer_read_mask, shift_width);
  uintmax_t set_mask = 1 << (uint8_t)(shift_width + SWCOUNT_SLOT_EXP);

  result.view._raw = view._raw | set_mask;
  result.position = shift_width;
  return result;
}

bool release_read_lock(MSWAP_Count *mswap, CtrlView_Count src_view, uint32_t read_slot, MSWAP_Stat *stats) {
  __auto_type ctrl_ptr = &(mswap->control.raw);
  CtrlView_Count dst_view;

#if MORE_ATO_LESS_CONTENTION > 0
  src_view._raw = atomic_load_explicit(ctrl_ptr, memory_order_acquire);
#endif
  LockDesc desc = lock_descriptor_at(src_view, read_slot);

  CAS_LOOP_FOR(MAX_RELEASE_SWAP_ATTEMPTS, is_swap_ok) {
    ASSERT(desc.lock > 0, "Lock was released more than once");
    dst_view = src_view;
    desc.lock -= 1;
    set_version_lock(&dst_view, desc);

    bool is_swapped = atomic_compare_exchange_weak_explicit(ctrl_ptr, &src_view._raw, dst_view._raw,
                                                            MEMORY_ORDER_REL_LOCK, memory_order_relaxed);
    if (is_swapped)
      break;

    desc = lock_descriptor_at(src_view, desc.version);
    stats->release_tries += 1;
  }
  return is_swap_ok;
}

bool release_write_lock(MSWAP_Count *mswap, CtrlView_Count src_view, uint32_t writer_slot, MSWAP_Stat *stats) {
  __auto_type ctrl_ptr = &(mswap->control.raw);
  CtrlView_Count dst_view;
  uintmax_t clear_mask = ~(1 << (uint8_t)(writer_slot + SWCOUNT_SLOT_EXP));

#if MORE_ATO_LESS_CONTENTION > 0
  src_view._raw = atomic_load_explicit(ctrl_ptr, memory_order_acquire);
#endif

  CAS_LOOP_FOR(MAX_RELEASE_SWAP_ATTEMPTS, is_swap_ok) {
    ASSERT(src_view._raw & (~clear_mask), "Lock was released more than once");
    dst_view._raw = src_view._raw & clear_mask;
    dst_view.latest_slot = writer_slot;

    bool is_swapped = atomic_compare_exchange_weak_explicit(ctrl_ptr, &src_view._raw, dst_view._raw,
                                                            MEMORY_ORDER_REL_LOCK, memory_order_relaxed);
    if (is_swapped)
      break;
    stats->release_tries += 1;
  }
  return is_swap_ok;
}

void evaluate_and_wait_under_contention(MSWAP_Stat *stats, int is_read) {
#if WAIT_QUANTUM > 0
  uint32_t score = stats->acquire_tries / (is_read ? 2 : 1)
                   + (stats->all_lock_taken + stats->version_overflow) * (is_read ? 2 : 3);
  stats->wait_spins += busy_spin_for(score * WAIT_QUANTUM);
#endif
}

//////////////////////////////////////////////////////////////////////////////////////////////

uintmax_t version_lock_mask() {
  uintmax_t mask = 0;
  uintmax_t floating_mask = SWCOUNT_LAST_READER << (uint8_t)(SWCOUNT_SLOT_EXP + SWCOUNT_SLOTS);
  for (uint32_t i = 0; i < SWCOUNT_VERSION_COUNT; ++i) {
    mask |= floating_mask;
    floating_mask = floating_mask << (uint8_t)(SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP);
  }
  return mask;
}

uint32_t version_lock_slots_taken(CtrlView_Count view, uint32_t *slots) {
  uint32_t slot_taken_count = 0;
  for (uint32_t i = 0; i < SWCOUNT_VERSION_COUNT; ++i) {
    if (!(view.version_locks & SWCOUNT_LAST_READER))
      continue;

    view.version_locks >>= (uint8_t)SWCOUNT_READER_EXP;
    slots[i] = view.version_locks & SWCOUNT_LAST_SLOT;
    view.version_locks >>= (uint8_t)SWCOUNT_SLOT_EXP;
    slot_taken_count++;
  }
  return slot_taken_count;
}

void purge_all_version_locks(CtrlView_Count *view) {
  uintmax_t purge_mask = ~version_lock_mask();
  __auto_type ctrl_ptr = &(view->raw);
  // LOG_DEBUG("Purging read locks %lx", 0lu + old_value.version_locks);
  atomic_fetch_and(ctrl_ptr, purge_mask);
  // LOG_DEBUG("Purging read locks (end) %lx", 0lu + new_value.version_locks);
}

uintmax_t writer_mask_for_all_reading_slots(CtrlView_Count view) {
  uintmax_t mask = 0;
  for (uint32_t i = 0; i < SWCOUNT_VERSION_COUNT; ++i) {
    uintmax_t raw_lock = view.version_locks >> (uint8_t)(i * (SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP));
    uint8_t slot = (raw_lock >> (uint8_t)SWCOUNT_READER_EXP) & SWCOUNT_LAST_SLOT;
    uintmax_t reading = raw_lock & SWCOUNT_LAST_READER;
    mask |= (reading ? (1 << slot) : 0);
  }
  return mask;
}

// static uintmax_t __negatives__[] = {
//  ~(((SWCOUNT_LAST_SLOT << (uint8_t)SWCOUNT_READER_EXP) + SWCOUNT_LAST_READER) << (0*(SWCOUNT_READER_EXP +
//  SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP)),
//  ~(((SWCOUNT_LAST_SLOT << (uint8_t)SWCOUNT_READER_EXP) + SWCOUNT_LAST_READER) << (1*(SWCOUNT_READER_EXP +
//  SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP)),
//  ~(((SWCOUNT_LAST_SLOT << (uint8_t)SWCOUNT_READER_EXP) + SWCOUNT_LAST_READER) << (2*(SWCOUNT_READER_EXP +
//  SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP)),
//  ~(((SWCOUNT_LAST_SLOT << (uint8_t)SWCOUNT_READER_EXP) + SWCOUNT_LAST_READER) << (3*(SWCOUNT_READER_EXP +
//  SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP)),
//};
// static uint8_t __shifts__[] = {
//  0*(SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP,
//  1*(SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP,
//  2*(SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP,
//  3*(SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP)+SWCOUNT_SLOTS+SWCOUNT_SLOT_EXP,
//};
void set_version_lock(CtrlView_Count *dst_view, LockDesc desc) {
  uintmax_t negative = (SWCOUNT_LAST_SLOT << (uint8_t)SWCOUNT_READER_EXP) + SWCOUNT_LAST_READER;
  negative = negative << (uint8_t)(desc.version * (SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP));
  negative = ~negative;
  uintmax_t mask = desc.lock + (desc.slot << (uint8_t)SWCOUNT_READER_EXP);
  mask = mask << (uint8_t)(desc.version * (SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP));
  /// mask = mask << __shifts__[desc.version];

  dst_view->version_locks &= negative;
  dst_view->version_locks |= mask;
  /// dst_view->_raw &= __negatives__[desc.version];
  /// dst_view->_raw |= mask;
}

LockDesc lock_descriptor_at(CtrlView_Count view, uint32_t version_idx) {
  LockDesc desc = {.version = version_idx };
  desc.lock = view.version_locks >> (uint8_t)(version_idx * (SWCOUNT_READER_EXP + SWCOUNT_SLOT_EXP));
  /// desc.lock = view._raw >> __shifts__[version_idx];
  desc.slot = desc.lock >> (uint8_t)SWCOUNT_READER_EXP;
  desc.lock &= SWCOUNT_LAST_READER;
  desc.slot &= SWCOUNT_LAST_SLOT;
  return desc;
}
