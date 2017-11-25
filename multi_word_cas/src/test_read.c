#include <test_read.h>

#include <pthread.h>
#include <stdatomic.h>
#include <stdlib.h>

#include <common.h>
#include <logger.h>
#include <multi_cas_noaba.h>
#include <util.h>

UNIT_TEST_START(test_mask_calculation)
  CtrlView_NoABA view;
  // version_mask = 0b 0000 1111 1110 0001 1111 1100 0011 1111 1000 0111 1111 0000 0000 0000 0000 0000
  uintmax_t version_mask = version_lock_mask();
  view._raw = version_mask;
  TEST_ASSERT(version_mask == 0x0fe1fc3f87f00000, "Version lock mask calculation is wrong %lx", version_mask);
  TEST_ASSERT(view.latest_slot == 0, "Union layout is wrong");
  TEST_ASSERT(view.writer_mask == 0, "Union layout is wrong 2");
  TEST_ASSERT(view.version_locks != 0, "Union layout is wrong 3");

  view._raw = NOABA_WRITER_MASK;
  TEST_ASSERT(NOABA_WRITER_MASK == 0xffff0, "Writer mask calculation is wrong %llx", NOABA_WRITER_MASK);
  TEST_ASSERT(view.latest_slot == 0, "Union layout is wrong 4");
  TEST_ASSERT(view.writer_mask == NOABA_ALL_WRITERS, "Union layout is wrong 5");
  TEST_ASSERT(view.version_locks == 0, "Union layout is wrong 6");
UNIT_TEST_END

UNIT_TEST_START(test_lock_descriptor_at)
  CtrlView_NoABA view = { 0 };
  FOREACH_VERSION_LOCK(view, desc) {
    TEST_ASSERT(desc.slot == 0 && desc.lock == 0, "Failed to read empty lock at %d", desc.version);
  }
  view.raw |= version_lock_mask();
  FOREACH_VERSION_LOCK(view, desc) {
    TEST_ASSERT(desc.slot == 0 && desc.lock == NOABA_LAST_READER, "Failed to read full lock at %d", desc.version);
  }
UNIT_TEST_END

UNIT_TEST_START(test_set_version_lock)
  for (uint32_t lock_count = 0; lock_count < NOABA_READERS; ++lock_count) {
    CtrlView_NoABA view = { 0 };
    FOREACH_VERSION_LOCK(view, desc) {
      desc.slot = 1;
      desc.lock = lock_count;
      set_version_lock(&view, desc);

      LockDesc read_desc = { 0 };
      read_desc = lock_descriptor_at(view, desc.version);
      TEST_ASSERT(read_desc.slot == 1 && read_desc.lock == lock_count, "Failed to set lock descriptor at %d/%d",
                  desc.version, lock_count);
    }
  }
  for (uint32_t slot_idx = 0; slot_idx < NOABA_SLOTS; ++slot_idx) {
    CtrlView_NoABA view = { 0 };
    FOREACH_VERSION_LOCK(view, desc) {
      desc.slot = slot_idx;
      desc.lock = 1;
      set_version_lock(&view, desc);

      LockDesc read_desc = { 0 };
      read_desc = lock_descriptor_at(view, desc.version);
      TEST_ASSERT(read_desc.slot == slot_idx && read_desc.lock == 1, "Failed to set lock descriptor at %d/%d",
                  desc.version, slot_idx);
    }
  }
UNIT_TEST_END

UNIT_TEST_START(test_purge_all_version_locks)
  CtrlView_NoABA view = { 0 };
  FOREACH_VERSION_LOCK(view, desc) {
    desc.slot = NOABA_LAST_SLOT;
    desc.lock = NOABA_LAST_READER;
    set_version_lock(&view, desc);
  }
  purge_all_version_locks(&view);
  FOREACH_VERSION_LOCK(view, desc) {
    TEST_ASSERT(desc.slot == NOABA_LAST_SLOT, "Purge removed slot values");
    TEST_ASSERT(desc.lock == 0, "Purge did not clear version lock");
  }
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_increment_latest_reader_lock)
  CtrlView_NoABA view = { 0 };
  MCAS_Stat stats = { 0 };
  LockResult inc_res = increment_latest_reader_lock(view, &stats);
  view = inc_res.view;
  TEST_ASSERT(inc_res.success                                                                       //
                  && lock_descriptor_at(view, 0).lock == 1 && lock_descriptor_at(view, 0).slot == 0 //
                  && stats.version_overflow == 0 && stats.all_lock_taken == 0,
              "Failed to increment lock on zero ctrl view %lx", view.raw);

  view.latest_slot = 1;
  inc_res = increment_latest_reader_lock(view, &stats);
  view = inc_res.view;
  // the algorithm will take the last free version lock
  TEST_ASSERT(inc_res.success                                                    //
                  && lock_descriptor_at(view, NOABA_VERSION_COUNT - 1).lock == 1 //
                  && lock_descriptor_at(view, NOABA_VERSION_COUNT - 1).slot == 1 //
                  && stats.version_overflow == 0                                 //
                  && stats.all_lock_taken == 0,
              "Failed to increment lock on zero ctrl view %lx", view.raw);

  inc_res = increment_latest_reader_lock(view, &stats);
  view = inc_res.view;
  TEST_ASSERT(inc_res.success //
                  && lock_descriptor_at(view, NOABA_VERSION_COUNT - 1).lock == 2,
              "Failed to increment again lock on zero ctrl view %lx", view.raw);

  LockDesc write_desc = {.slot = 0, .lock = NOABA_READER_HIGH_MARK, .version = 0 };
  view.latest_slot = 0;
  reset_stats(&stats);
  set_version_lock(&view, write_desc);
  inc_res = increment_latest_reader_lock(view, &stats);
  view = inc_res.view;
  TEST_ASSERT(!inc_res.success //
                  && lock_descriptor_at(view, 0).lock == NOABA_READER_HIGH_MARK && stats.version_overflow == 1,
              "Failed to notice version read lock was full %lx", view.raw);

  FOREACH_VERSION_LOCK(view, desc) {
    desc.slot = 0;
    desc.lock = 1;
    set_version_lock(&view, desc);
  }

  view.latest_slot = 1;
  inc_res = increment_latest_reader_lock(view, &stats);
  view = inc_res.view;
  TEST_ASSERT(!inc_res.success && stats.all_lock_taken == 1, "Failed to notice all version lock were taken %lx",
              view.raw);
UNIT_TEST_END

UNIT_TEST_START(test_lock_for_reading)
  MCas_NoABA mcas = { 0 };
  MCAS_Stat stats = { 0 };

  LockResult result = lock_for_read_or_write(&mcas, &stats, increment_latest_reader_lock, 1);
  TEST_ASSERT(result.view.raw == mcas.control.raw, "Copy should match source data");
  TEST_ASSERT(result.success, "Operation should have succeded");
  TEST_ASSERT(stats.acquire_tries == 0, "Buggy stat updates");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == 0 && desc.slot == 0 && desc.lock == 1)         //
                    || (desc.version != 0 && desc.slot == 0 && desc.lock == 0), //
                "Did not increment the right slot");
  }

  // Increment same slot again
  result = lock_for_read_or_write(&mcas, &stats, increment_latest_reader_lock, 1);
  TEST_ASSERT(result.view.raw == mcas.control.raw, "Copy should match source data");
  TEST_ASSERT(result.success, "Operation should have succeded");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == 0 && desc.slot == 0 && desc.lock == 2)         //
                    || (desc.version != 0 && desc.slot == 0 && desc.lock == 0), //
                "Did not increment the right slot");
  }

  // Increment another slot
  mcas.control.latest_slot = 1;
  result = lock_for_read_or_write(&mcas, &stats, increment_latest_reader_lock, 1);
  TEST_ASSERT(result.view.raw == mcas.control.raw, "Copy should match source data");
  TEST_ASSERT(result.success, "Operation should have succeded");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.slot == 1 && desc.lock == 1)                              //
                    || (desc.version == 0 && desc.slot == 0 && desc.lock == 2)  //
                    || (desc.version != 0 && desc.slot == 0 && desc.lock == 0), //
                "Did not increment the right slot");
  }
UNIT_TEST_END

UNIT_TEST_START(test_lock_for_reading_failure)
  MCas_NoABA mcas = { 0 };
  MCAS_Stat stats = { 0 };

  // All version are taken by other version
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    desc.slot = 1;
    desc.lock = 1;
    set_version_lock(&mcas.control, desc);
  }

  LockResult result = lock_for_read_or_write(&mcas, &stats, increment_latest_reader_lock, 1);
  TEST_ASSERT(!result.success, "Operation should have failed");
  TEST_ASSERT(stats.acquire_tries > 0, "Stats not updated");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.slot == 1 && desc.lock == 1, "Failure the lock should have not modified mcas");
  }

  // The slot to read is already full with readers
  mcas.control.latest_slot = 2;
  LockDesc full_desc = {.slot = 2, .lock = NOABA_READER_HIGH_MARK, .version = 0 };
  set_version_lock(&mcas.control, full_desc);

  result = lock_for_read_or_write(&mcas, &stats, increment_latest_reader_lock, 1);
  TEST_ASSERT(!result.success, "Operation should have failed");
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.slot == 1 && desc.lock == 1) //
                    || (desc.slot == 2 && desc.lock == NOABA_READER_HIGH_MARK),
                "Failure the lock should have not modified mcas");
  }
UNIT_TEST_END

UNIT_TEST_START(test_release_read_lock)
  MCas_NoABA mcas = { 0 };
  MCAS_Stat stats = { 0 };

  // set all version locks to slot 2 safe one that will lock slot 0
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    desc.slot = 2;
    desc.lock = 0;
    set_version_lock(&mcas.control, desc);
  }
  LockDesc lock_slot0 = {.slot = 0, .lock = 2, .version = 1 };
  set_version_lock(&mcas.control, lock_slot0);

  CtrlView_NoABA snap = mcas.control;
  bool success = release_read_lock(&mcas, snap, lock_slot0.version, &stats);

  TEST_ASSERT(success, "Operation should have succeded");
  TEST_ASSERT(stats.release_tries == 0, "Buggy stat updates");
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == lock_slot0.version && desc.lock == 1 && desc.slot == 0) //
                    || (desc.version != lock_slot0.version && desc.lock == 0 && desc.slot == 2),
                "Decremented read lock badly");
  }

  // Decrement same slot again
  snap = mcas.control;
  success = release_read_lock(&mcas, snap, lock_slot0.version, &stats);

  TEST_ASSERT(success, "Operation should have succeded");
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == lock_slot0.version && desc.lock == 0 && desc.slot == 0) //
                    || (desc.version != lock_slot0.version && desc.lock == 0 && desc.slot == 2),
                "Decremented read lock badly");
  }
UNIT_TEST_END

UNIT_TEST_START(test_release_read_lock_retry)
  MCas_NoABA mcas = { 0 };
  MCAS_Stat stats = { 0 };

  LockDesc lock_slot0 = {.slot = 0, .lock = 2, .version = 0 };
  set_version_lock(&mcas.control, lock_slot0);

  // Simulate a concurrent writer by moving the latest slot
  CtrlView_NoABA snap = mcas.control;
  mcas.control.latest_slot = 1;
  bool success = release_read_lock(&mcas, snap, lock_slot0.version, &stats);

  TEST_ASSERT(success, "Operation should have succeded");
  TEST_ASSERT(stats.release_tries <= 1, "Buggy stat updates : %d", stats.release_tries);

  LockDesc desc = lock_descriptor_at(mcas.control, lock_slot0.version);
  TEST_ASSERT(desc.slot == 0 && desc.lock == 1, "Decremented read lock badly");
UNIT_TEST_END

UNIT_TEST_START(test_emergency_version_lock_draining)
  MCas_NoABA mcas = { 0 };
  LockDesc lock_slot2 = {.slot = 2, .lock = 2, .version = 1 };
  mcas.control.latest_slot = lock_slot2.slot;

  set_version_lock(&mcas.control, lock_slot2);
  uint32_t to_drain = emergency_version_lock_draining(&mcas, mcas.control, lock_slot2.version);

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.version == lock_slot2.version || //
                    (desc.lock == 0 && desc.slot == 0),
                "Could not block version lock for draining 1 : %lx", mcas.control._raw);
    TEST_ASSERT(desc.version != lock_slot2.version || //
                    (desc.lock == NOABA_LAST_READER && desc.slot == lock_slot2.slot),
                "Could not block version lock for draining 2");
  }
  TEST_ASSERT(to_drain == lock_slot2.lock, "Bad calculation of left to drain");
UNIT_TEST_END

UNIT_TEST_START(test_emergency_version_lock_draining_retry_cas)
  MCas_NoABA mcas = { 0 };

  LockDesc lock_slot0 = {.slot = 0, .lock = 3, .version = 0 };
  set_version_lock(&mcas.control, lock_slot0);
  CtrlView_NoABA old_view = mcas.control;
  mcas.control.latest_slot = 1;
  mcas.control.writer_mask = 1;
  uint32_t to_drain = emergency_version_lock_draining(&mcas, old_view, lock_slot0.version);

  TEST_ASSERT(to_drain == lock_slot0.lock, "Bad calculation of left to drain");
  TEST_ASSERT(mcas.control.latest_slot == 1 && mcas.control.writer_mask == 1,
              "Draining version lock should not change other fields");
UNIT_TEST_END

UNIT_TEST_START(test_emergency_version_lock_draining_contention)
  MCas_NoABA mcas = { 0 };

  LockDesc lock_slot0 = {.slot = 0, .lock = 3, .version = 0 };
  set_version_lock(&mcas.control, lock_slot0);
  CtrlView_NoABA old_view = mcas.control;
  lock_slot0.lock = NOABA_LAST_READER;
  set_version_lock(&mcas.control, lock_slot0);
  uint32_t to_drain = emergency_version_lock_draining(&mcas, old_view, lock_slot0.version);

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.version == lock_slot0.version || //
                    (desc.lock == 0 && desc.slot == 0),
                "Could not block version lock for draining 1");
    TEST_ASSERT(desc.version != lock_slot0.version || //
                    (desc.lock == NOABA_LAST_READER && desc.slot == lock_slot0.slot),
                "Could not block version lock for draining 2");
  }
  TEST_ASSERT(to_drain == 0, "Failed to detect contention to drain");
UNIT_TEST_END


//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_noaba_mcas_creation)
  void *content = malloc(sizeof(TestContentSlots));
  MCas_NoABA mcas = build_mcas_noaba(content);
  TEST_ASSERT(content == mcas.content, "bad creation");
  TEST_ASSERT(mcas.control._raw == 0, "bad creation");

  LockDesc desc = {.slot = 0, .lock = 1, .version = 0 };
  set_version_lock(&mcas.control, desc);
  /* this should break */
  // free_mcas_noaba(&mcas);

  if (mcas.content)
    free(mcas.content);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_simple)
  TestContentSlots content = { { 0 } };
  TestContent read_dst = {.x = 1, .y = 2, .z = 3 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  MCAS_Stat stats = noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  // mcas.control should go back to 0 since latest_slot == 0
  TEST_ASSERT(read_dst.x == 0 && read_dst.y == 0 && read_dst.z == 0 && mcas.control.raw == 0,
              "Could not read without contention, ctrl %lx", mcas.control.raw);
  TEST_ASSERT(stats.success && stats.acquire_tries == 0 && stats.release_tries == 0 && stats.wait_spins == 0
                  && stats.version_overflow == 0 && stats.all_lock_taken == 0,
              "Buggy stats acquisition");

  // Test read idempotent
  read_dst.x = 333;
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  TEST_ASSERT(read_dst.x == 0 && read_dst.y == 0 && read_dst.z == 0 && mcas.control.raw == 0,
              "Could not read without contention, ctrl %lx", mcas.control.raw);

  // Test read another slot
  mcas.control.latest_slot = 1;
  content[1].x = 666;
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  TEST_ASSERT(read_dst.x == 666 && read_dst.y == 0 && read_dst.z == 0 //
                  && mcas.control.raw != 0,                           //
              "Could not read without contention slot 1, ctrl %lx", mcas.control.raw);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_one_version_already_taken)
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  mcas.control.latest_slot = 1;

  LockDesc dummy_lock_ver = {.slot = 3, .lock = 1, .version = 0 };
  set_version_lock(&mcas.control, dummy_lock_ver);

  // latest slot is not 0 so dummy_lock_ver should NOT be clobbered by this read
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  LockDesc check_not_clobber = lock_descriptor_at(mcas.control, 0);
  TEST_ASSERT(check_not_clobber.slot == 3 && check_not_clobber.lock == 1, //
              "Over decremented version lock, ctrl %lx", mcas.control.raw);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_target_slot_already_readers)
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  mcas.control.latest_slot = 1;

  LockDesc dummy_lock_ver = {.slot = 1, .lock = 1, .version = 0 };
  set_version_lock(&mcas.control, dummy_lock_ver);

  // There is already a reader locking slot 1, it should be reused
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.slot == 1 && desc.lock == 1 && desc.version == dummy_lock_ver.version) //
                    || (desc.slot == 0 && desc.lock == 0 && desc.version != dummy_lock_ver.version),
                "Did not recycle version lock , ctrl %lx", mcas.control.raw);
  }
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_version_recycling)
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  mcas.control.latest_slot = 1;

  // None of the version locks point to slot 1, we recycle the last free
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  LockDesc desc_read_slot = lock_descriptor_at(mcas.control, NOABA_VERSION_COUNT - 1);

  // The last version lock points already to the slot we read, recycle it
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.slot == 1 && desc.version == desc_read_slot.version)         //
                    || (desc.slot == 0 && desc.version != desc_read_slot.version), //
                "Did not recycle version lock (1.2), ctrl %lx", mcas.control.raw);
    TEST_ASSERT(desc.lock == 0, "Did not recycle version lock (1.3), ctrl %lx", mcas.control.raw);
  }

  // All version locks have no readers, we should recycle the last one
  mcas.control.latest_slot = 2;

  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  desc_read_slot = lock_descriptor_at(mcas.control, NOABA_VERSION_COUNT - 1);

  TEST_ASSERT(desc_read_slot.slot == 2 && desc_read_slot.lock == 0, "Did not recycle version lock (2), ctrl %lx",
              mcas.control.raw);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_latest_slot_no_more_readers)
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);

  LockDesc fill_up_slot0 = {.slot = 0, .lock = NOABA_READER_HIGH_MARK, .version = 0 };
  set_version_lock(&mcas.control, fill_up_slot0);

  MCAS_Stat stats = noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  TEST_ASSERT(!stats.success, "Cannot read when version lock is full");
  TEST_ASSERT(stats.acquire_tries == MAX_ACQUIRE_CAS_ATTEMPTS && stats.release_tries == 0 && stats.wait_spins > 0
                  && stats.version_overflow == MAX_ACQUIRE_CAS_ATTEMPTS && stats.all_lock_taken == 0,
              "Buggy stats acquisition");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.slot == 0, "An unsuccessful operation should not change anything");
    TEST_ASSERT((desc.version == fill_up_slot0.version && desc.lock == fill_up_slot0.lock) //
                    || (desc.version != fill_up_slot0.version && desc.lock == 0),          //
                "An unsuccessful operation should not change anything");
  }
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_all_version_slots_taken)
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    desc.slot = 2;
    desc.lock = 1;
    set_version_lock(&mcas.control, desc);
  }

  MCAS_Stat stats = noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  TEST_ASSERT(!stats.success, "Cannot read when all version locks are taken by other slots");
  TEST_ASSERT(stats.acquire_tries == MAX_ACQUIRE_CAS_ATTEMPTS && stats.release_tries == 0 && stats.wait_spins > 0
                  && stats.version_overflow == 0 && stats.all_lock_taken == MAX_ACQUIRE_CAS_ATTEMPTS,
              "Buggy stats acquisition");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.slot == 2 && desc.lock == 1, "An unsuccessful operation should not change anything");
  }
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

static void *read_mcas_in_a_loop(void *params_raw) {
  HelperParams *params = (HelperParams *)params_raw;
  MCas_NoABA *mcas = params->mcas;

  for (uint32_t thr_iterations = params->thr_iterations; thr_iterations; --thr_iterations) {
    MCAS_Stat one_run_stat = noaba_cas_read(mcas, params->target, test_content_reader_func);
    mcas_stats_merge(params->stats, &one_run_stat);

    if (!one_run_stat.success) {
      pthread_yield();
      if (one_run_stat.to_drain)
        purge_all_version_locks(&mcas->control);
    }
    else if (params->wait_for)
      busy_spin_for(params->wait_for);
  }
  return params_raw;
}

static CtrlView_NoABA adjust_latest_slot(CtrlView_NoABA old_value, uint32_t new_slot) {
  CtrlView_NoABA new_value = old_value;
  new_value.latest_slot = new_slot;
  return new_value;
}

static void *dummy_latest_slot_changer(void *params_raw) {
  HelperParams *params = (HelperParams *)params_raw;
  CtrlView_NoABA *live_value = &(params->mcas->control);
  uint32_t from_slot = params->cur_slot;
  uint32_t cur_slot = from_slot;
  uint32_t iterations = 0;

  while (pthread_mutex_trylock(&(params->stop))
         && (!params->thr_iterations || (iterations < params->thr_iterations))) {
    iterations += 1;
    cur_slot += 1;
    if (cur_slot == params->to_slot)
      cur_slot = from_slot;

    CtrlView_NoABA old_value;
    old_value._raw = atomic_load(&old_value.raw);

    for (CtrlView_NoABA new_value = adjust_latest_slot(old_value, cur_slot);              //
         !atomic_compare_exchange_weak(&live_value->raw, &old_value._raw, new_value.raw); //
         new_value = adjust_latest_slot(old_value, cur_slot))
      ;
    if (params->wait_for)
      busy_spin_for(params->wait_for);
  }

  if (!params->thr_iterations || (iterations < params->thr_iterations))
    pthread_mutex_unlock(&(params->stop));
  params->cur_slot = cur_slot;
  return params_raw;
}

UNIT_TEST_START(test_noaba_cas_read_mt_simple)
  MT_MCAS_BOILER_PLATE(1, 1, mcas, read_dst, stats, params);
  MT_KICKOFF_MCAS_THREADS(reader_thr, read_dst, stats, params, read_mcas_in_a_loop);
  MT_JOIN_MCAS_THREADS(reader_thr, read_dst, stats, params);

  TEST_ASSERT(read_dst->x == 0 && read_dst->y == 0 && read_dst->z == 0 //
                  && mcas.control.raw == 0 && stats->acquire_tries == 0 && stats->release_tries == 0,
              "Could not read from another thread, ctrl %lx", mcas.control.raw);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_mt_many_simple)
  MT_MCAS_BOILER_PLATE(6, 10000, mcas, read_dst, stats, params);
  MT_KICKOFF_MCAS_THREADS(reader_thr, read_dst, stats, params, read_mcas_in_a_loop);
  MT_JOIN_MCAS_THREADS(reader_thr, read_dst, stats, params);

  FOREACH_MCAS_THREAD(thr, read_dst, stats, params) {
    TEST_ASSERT(read_dst_i->x == 0 && read_dst_i->y == 0 && read_dst_i->z == 0, //
                "Could not read slot 0, thread %d", thr);
    TEST_ASSERT(stats_i->version_overflow == 0, //
                "There are not enough readers to overflow version lock : %d", stats_i->version_overflow);
    TEST_ASSERT(stats_i->all_lock_taken == 0, "Version lock exhaustion is not possible");
  }

  TEST_ASSERT(mcas.control.latest_slot == 0, "Corrupted mcas state 1");
  TEST_ASSERT(mcas.control.writer_mask == 0, "Corrupted mcas state 2");
  TEST_ASSERT(mcas.control.version_locks == 0, "Corrupted mcas state 3");
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_mt_many_with_wait)
  MT_MCAS_BOILER_PLATE(6, 1000, mcas, read_dst, stats, params);
  for (uint32_t i = 0; i < max_mcas_thr; ++i) {
    params[i].wait_for = (WAIT_QUANTUM + i) * 10;
  }
  MT_KICKOFF_MCAS_THREADS(reader_thr, read_dst, stats, params, read_mcas_in_a_loop);
  MT_JOIN_MCAS_THREADS(reader_thr, read_dst, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_mt_slot_changing)
  MT_MCAS_BOILER_PLATE(4, 1000, mcas, read_dst, stats, params);
  MT_KICKOFF_MCAS_THREADS(reader_thr, read_dst, stats, params, read_mcas_in_a_loop);

  HelperParams write_params = {
    .mcas = &mcas,
    .cur_slot = 0,
    .to_slot = NOABA_VERSION_COUNT,
    .wait_for = WAIT_QUANTUM * 5,
    .stop = PTHREAD_MUTEX_INITIALIZER //
  };
  int stop_ok = pthread_mutex_trylock(&write_params.stop);
  TEST_ASSERT(stop_ok == 0, "Failed lock writer stop signal");

  pthread_t writer_thr;
  int creation_ok = pthread_create(&writer_thr, NULL, dummy_latest_slot_changer, &write_params);
  TEST_ASSERT(creation_ok == 0, "Failed to create writer thread");

  MT_JOIN_MCAS_THREADS(reader_thr, read_dst, stats, params);
  FOREACH_MCAS_THREAD(thr, read_dst, stats, params) {
    // LOG_DEBUG("%d: %lu, %lu, %lu", thr, read_dst_i->x, read_dst_i->y, read_dst_i->z);
    TEST_ASSERT(stats_i->all_lock_taken == 0, "Version lock exhaustion is not possible");
    TEST_ASSERT(read_dst_i->x == read_dst_i->y && read_dst_i->y == read_dst_i->z, //
                "Could not read from various slots, thread %d", thr);
  }

  pthread_mutex_unlock(&write_params.stop);
  int join_ok = join_all(&writer_thr, 1, 666);
  TEST_ASSERT(join_ok == 0, "Writer threads never completed");
  TEST_ASSERT(write_params.cur_slot == mcas.control.latest_slot, "Writer thread failed to change latest slot");
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_mt_contention_version_locks)
  MT_MCAS_BOILER_PLATE(NOABA_VERSION_COUNT + 1, 10000, mcas, read_dst, stats, params);
  // we pin all version locks
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    desc.slot = desc.version;
    desc.lock = 1;
    set_version_lock(&mcas.control, desc);
  }
  MT_KICKOFF_MCAS_THREADS(reader_thr, read_dst, stats, params, read_mcas_in_a_loop);

  HelperParams write_params = {
    .mcas = &mcas,
    .cur_slot = NOABA_VERSION_COUNT - 1,
    .to_slot = NOABA_VERSION_COUNT + 1,
    .wait_for = WAIT_QUANTUM * 2,
    .thr_iterations = mcas_rounds / 7,
    .stop = PTHREAD_MUTEX_INITIALIZER //
  };
  int stop_ok = pthread_mutex_trylock(&write_params.stop);
  TEST_ASSERT(stop_ok == 0, "Failed lock writer stop signal");

  pthread_t writer_thr;
  int creation_ok = pthread_create(&writer_thr, NULL, dummy_latest_slot_changer, &write_params);
  TEST_ASSERT(creation_ok == 0, "Failed to create writer thread");

  MT_JOIN_MCAS_THREADS(reader_thr, read_dst, stats, params);

  pthread_mutex_unlock(&write_params.stop);
  int join_ok = join_all(&writer_thr, 1, 666);
  TEST_ASSERT(join_ok == 0, "Writer threads never completed");
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_mt_latest_slot_almost_full)
  MT_MCAS_BOILER_PLATE(4, 1000, mcas, read_dst, stats, params);

  // only one more reader allowed
  LockDesc almost_full = {.slot = 3, .lock = NOABA_READER_LAST_MARK, .version = 0 };
  uint32_t slot_val = almost_full.slot;
  set_version_lock(&mcas.control, almost_full);
  mcas.control.latest_slot = almost_full.slot;

  MT_KICKOFF_MCAS_THREADS(reader_thr, read_dst, stats, params, read_mcas_in_a_loop);
  MT_JOIN_MCAS_THREADS(reader_thr, read_dst, stats, params);

  FOREACH_MCAS_THREAD(thr, read_dst, stats, params) {
    // this actually happens quite often
    // TEST_ASSERT(stats_i->acquire_tries != 0 && stats_i->version_overflow != 0,
    //            "If acquire_tries and version_overflow are 0 for a thread then there is starvation, %d", thr);
    TEST_ASSERT(stats_i->release_tries == 0,
                "Since only 1 is allowed to read each time, release should have not contention, %d", thr);
    TEST_ASSERT(read_dst_i->x == slot_val && read_dst_i->y == slot_val && read_dst_i->z == slot_val,
                "Could not read when read version lock is almost full, thread %d", thr);
  }
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_read_mt_all_version_slots_taken)
  MT_MCAS_BOILER_PLATE(4, 1, mcas, read_dst, stats, params);

  // no other reader allowed
  LockDesc almost_full = {.slot = 1, .lock = NOABA_READER_HIGH_MARK, .version = 0 };
  set_version_lock(&mcas.control, almost_full);
  mcas.control.latest_slot = almost_full.slot;

  MT_KICKOFF_MCAS_THREADS(reader_thr, read_dst, stats, params, read_mcas_in_a_loop);
  int join_ok = join_all(reader_thr, max_mcas_thr, mcas_rounds + 666);
  TEST_ASSERT(join_ok == 0, "Reader threads never completed");

  FOREACH_MCAS_THREAD(thr, read_dst, stats, params) {
    TEST_ASSERT(stats_i->version_overflow == MAX_ACQUIRE_CAS_ATTEMPTS,
                "All cas attempts should have been exhausted, %d", thr);
    TEST_ASSERT(stats_i->success == 0, "Expecting it to fail, %d", thr);
  }
UNIT_TEST_END
