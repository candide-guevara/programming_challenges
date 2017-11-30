#include <test.h>

#include <pthread.h>
#include <stdlib.h>

#include <common.h>
#include <logger.h>
#include <multi_cas_noaba.h>
#include <util.h>

void test_content_writer_func(void *src, uint32_t slot, void *dst) { test_content_reader_func(dst, slot, src); }

void test_content_reader_func(void *src, uint32_t slot, void *dst) {
  TestContent *src_content = (TestContent *)src + slot;
  TestContent *dst_content = (TestContent *)dst;
  dst_content->x = src_content->x;
  dst_content->y = src_content->y;
  dst_content->z = src_content->z;
}

//////////////////////////////////////////////////////////////////////////////////////////////

void test_mask_calculation() {
  CtrlView_NoABA view;
  TEST_ASSERT(NOABA_SLOTS == 16 && NOABA_READERS == 128 && NOABA_VERSION_COUNT == 4,
              "Compilation parameters do not match this test");

  // version_mask = 0b 0000 1111 1110 0001 1111 1100 0011 1111 1000 0111 1111 0000 0000 0000 0000 0000
  uintmax_t version_mask = version_lock_mask();
  view.raw = version_mask;
  TEST_ASSERT(version_mask == 0x0fe1fc3f87f00000, "Version lock mask calculation is wrong %lx", version_mask);
  TEST_ASSERT(view.fields.latest_slot == 0, "Union layout is wrong");
  TEST_ASSERT(view.fields.writer_mask == 0, "Union layout is wrong 2");
  TEST_ASSERT(view.fields.version_locks != 0, "Union layout is wrong 3");

  uintmax_t writer_mask = writer_slot_mask();
  view.raw = writer_mask;
  TEST_ASSERT(writer_mask == 0xffff0, "Writer mask calculation is wrong %lx", writer_mask);
  TEST_ASSERT(view.fields.latest_slot == 0, "Union layout is wrong 4");
  TEST_ASSERT(view.fields.writer_mask == (1 << NOABA_SLOTS) - 1, "Union layout is wrong 5");
  TEST_ASSERT(view.fields.version_locks == 0, "Union layout is wrong 6");
}

void test_lock_descriptor_at() {
  CtrlView_NoABA view = { 0 };
  FOREACH_VERSION_LOCK(view, desc) {
    TEST_ASSERT(desc.slot == 0 && desc.lock == 0, "Failed to read empty lock at %d", desc.version);
  }
  view.raw |= version_lock_mask();
  FOREACH_VERSION_LOCK(view, desc) {
    TEST_ASSERT(desc.slot == 0 && desc.lock == NOABA_READERS - 1, "Failed to read full lock at %d", desc.version);
  }
}

void test_set_version_lock() {
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
}

//////////////////////////////////////////////////////////////////////////////////////////////

void test_increment_latest_reader_lock() {
  CtrlView_NoABA view = { 0 };
  MCAS_Stat stats = { 0 };
  TEST_ASSERT(increment_latest_reader_lock(&view, &stats) && lock_descriptor_at(view, 0).lock == 1
                  && lock_descriptor_at(view, 0).slot == 0 && stats.version_overflow == 0 && stats.all_ver_taken == 0,
              "Failed to increment lock on zero ctrl view %lx", view.raw);

  view.fields.latest_slot = 1;
  // the algorithm will take the last free version lock
  TEST_ASSERT(increment_latest_reader_lock(&view, &stats)                        //
                  && lock_descriptor_at(view, NOABA_VERSION_COUNT - 1).lock == 1 //
                  && lock_descriptor_at(view, NOABA_VERSION_COUNT - 1).slot == 1 //
                  && stats.version_overflow == 0                                 //
                  && stats.all_ver_taken == 0,
              "Failed to increment lock on zero ctrl view %lx", view.raw);
  TEST_ASSERT(increment_latest_reader_lock(&view, &stats)
                  && lock_descriptor_at(view, NOABA_VERSION_COUNT - 1).lock == 2,
              "Failed to increment again lock on zero ctrl view %lx", view.raw);

  LockDesc write_desc = {.slot = 0, .lock = NOABA_READERS - 1, .version = 0 };
  view.fields.latest_slot = 0;
  reset_stats(&stats);
  set_version_lock(&view, write_desc);
  TEST_ASSERT(!increment_latest_reader_lock(&view, &stats) && lock_descriptor_at(view, 0).lock == NOABA_READERS - 1
                  && stats.version_overflow == 1,
              "Failed to notice version read lock was full %lx", view.raw);

  FOREACH_VERSION_LOCK(view, desc) {
    desc.slot = 0;
    desc.lock = 1;
    set_version_lock(&view, desc);
  }
  view.fields.latest_slot = 1;
  TEST_ASSERT(!increment_latest_reader_lock(&view, &stats) && stats.all_ver_taken == 1,
              "Failed to notice all version lock were taken %lx", view.raw);
}

void test_lock_for_reading() {
  MCas_NoABA mcas = { 0 };
  MCAS_Stat stats = { 0 };

  LockResult result = lock_for_reading(&mcas, &stats);
  TEST_ASSERT(result.view.raw == mcas.control.raw, "Copy should match source data");
  TEST_ASSERT(result.success, "Operation should have succeded");
  TEST_ASSERT(stats.acquire_tries == 0, "Buggy stat updates");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == 0 && desc.slot == 0 && desc.lock == 1)         //
                    || (desc.version != 0 && desc.slot == 0 && desc.lock == 0), //
                "Did not increment the right slot");
  }

  // Increment same slot again
  result = lock_for_reading(&mcas, &stats);
  TEST_ASSERT(result.view.raw == mcas.control.raw, "Copy should match source data");
  TEST_ASSERT(result.success, "Operation should have succeded");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == 0 && desc.slot == 0 && desc.lock == 2)         //
                    || (desc.version != 0 && desc.slot == 0 && desc.lock == 0), //
                "Did not increment the right slot");
  }

  // Increment another slot
  mcas.control.fields.latest_slot = 1;
  result = lock_for_reading(&mcas, &stats);
  TEST_ASSERT(result.view.raw == mcas.control.raw, "Copy should match source data");
  TEST_ASSERT(result.success, "Operation should have succeded");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.slot == 1 && desc.lock == 1)                              //
                    || (desc.version == 0 && desc.slot == 0 && desc.lock == 2)  //
                    || (desc.version != 0 && desc.slot == 0 && desc.lock == 0), //
                "Did not increment the right slot");
  }
}

void test_lock_for_reading_failure() {
  MCas_NoABA mcas = { 0 };
  MCAS_Stat stats = { 0 };

  // All version are taken by other version
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    desc.slot = 1;
    desc.lock = 1;
    set_version_lock(&mcas.control, desc);
  }

  LockResult result = lock_for_reading(&mcas, &stats);
  TEST_ASSERT(!result.success, "Operation should have failed");
  TEST_ASSERT(stats.acquire_tries > 0, "Stats not updated");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.slot == 1 && desc.lock == 1, "Failure the lock should have not modified mcas");
  }

  // The slot to read is already full with readers
  mcas.control.fields.latest_slot = 2;
  LockDesc full_desc = {.slot = 2, .lock = NOABA_READERS - 1, .version = 0 };
  set_version_lock(&mcas.control, full_desc);

  result = lock_for_reading(&mcas, &stats);
  TEST_ASSERT(!result.success, "Operation should have failed");
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.slot == 1 && desc.lock == 1) //
                    || (desc.slot == 2 && desc.lock == NOABA_READERS - 1),
                "Failure the lock should have not modified mcas");
  }
}

void test_release_read_lock() {
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
  bool success = release_read_lock(&mcas, snap, &stats);

  TEST_ASSERT(success, "Operation should have succeded");
  TEST_ASSERT(stats.release_tries == 0, "Buggy stat updates");
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == lock_slot0.version && desc.lock == 1 && desc.slot == 0) //
                    || (desc.version != lock_slot0.version && desc.lock == 0 && desc.slot == 2),
                "Decremented read lock badly");
  }

  // Decrement same slot again
  snap = mcas.control;
  success = release_read_lock(&mcas, snap, &stats);

  TEST_ASSERT(success, "Operation should have succeded");
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.version == lock_slot0.version && desc.lock == 0 && desc.slot == 0) //
                    || (desc.version != lock_slot0.version && desc.lock == 0 && desc.slot == 2),
                "Decremented read lock badly");
  }
}

void test_release_read_lock_retry() {
  MCas_NoABA mcas = { 0 };
  MCAS_Stat stats = { 0 };

  LockDesc lock_slot0 = {.slot = 0, .lock = 2, .version = 0 };
  set_version_lock(&mcas.control, lock_slot0);

  // Simulate a concurrent writer by moving the latest slot
  CtrlView_NoABA snap = mcas.control;
  mcas.control.fields.latest_slot = 1;
  bool success = release_read_lock(&mcas, snap, &stats);

  TEST_ASSERT(success, "Operation should have succeded");
  TEST_ASSERT(stats.release_tries == 1, "Buggy stat updates");

  LockDesc desc = lock_descriptor_at(mcas.control, lock_slot0.version);
  TEST_ASSERT(desc.slot == 0 && desc.lock == 1, "Decremented read lock badly");
}

//////////////////////////////////////////////////////////////////////////////////////////////

void test_noaba_mcas_creation() {
  void *content = malloc(sizeof(TestContentSlots));
  MCas_NoABA mcas = build_mcas_noaba(content);
  free_mcas_noaba(&mcas);

  content = malloc(sizeof(TestContentSlots));
  mcas = build_mcas_noaba(content);
  LockDesc desc = {.slot = 0, .lock = 1, .version = 0 };
  set_version_lock(&mcas.control, desc);
  /* this should break */
  // free_mcas_noaba(&mcas);
  if (mcas.content)
    free(mcas.content);
}

void test_noaba_cas_read_simple() {
  TestContentSlots content = { { 0 } };
  TestContent read_dst = {.x = 1, .y = 2, .z = 3 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  MCAS_Stat stats = noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  // mcas.control should go back to 0 since latest_slot == 0
  TEST_ASSERT(read_dst.x == 0 && read_dst.y == 0 && read_dst.z == 0 && mcas.control.raw == 0,
              "Could not read without contention, ctrl %lx", mcas.control.raw);
  TEST_ASSERT(stats.success && stats.acquire_tries == 0 && stats.release_tries == 0 && stats.wait_spins == 0
                  && stats.version_overflow == 0 && stats.all_ver_taken == 0,
              "Buggy stats acquisition");

  // Test read idempotent
  read_dst.x = 333;
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  TEST_ASSERT(read_dst.x == 0 && read_dst.y == 0 && read_dst.z == 0 && mcas.control.raw == 0,
              "Could not read without contention, ctrl %lx", mcas.control.raw);

  // Test read another slot
  mcas.control.fields.latest_slot = 1;
  content[1].x = 666;
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  TEST_ASSERT(read_dst.x == 666 && read_dst.y == 0 && read_dst.z == 0 //
                  && mcas.control.raw != 0,                           //
              "Could not read without contention slot 1, ctrl %lx", mcas.control.raw);
}

void test_noaba_cas_read_one_version_already_taken() {
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  mcas.control.fields.latest_slot = 1;

  LockDesc dummy_lock_ver = {.slot = 3, .lock = 1, .version = 0 };
  set_version_lock(&mcas.control, dummy_lock_ver);

  // latest slot is not 0 so dummy_lock_ver should NOT be clobbered by this read
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);

  LockDesc check_not_clobber = lock_descriptor_at(mcas.control, 0);
  TEST_ASSERT(check_not_clobber.slot == 3 && check_not_clobber.lock == 1, //
              "Over decremented version lock, ctrl %lx", mcas.control.raw);
}

void test_noaba_cas_read_target_slot_already_readers() {
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  mcas.control.fields.latest_slot = 1;

  LockDesc dummy_lock_ver = {.slot = 1, .lock = 1, .version = 0 };
  set_version_lock(&mcas.control, dummy_lock_ver);

  // There is already a reader locking slot 1, it should be reused
  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT((desc.slot == 1 && desc.lock == 1 && desc.version == dummy_lock_ver.version) //
                    || (desc.slot == 0 && desc.lock == 0 && desc.version != dummy_lock_ver.version),
                "Did not recycle version lock , ctrl %lx", mcas.control.raw);
  }
}

void test_noaba_cas_read_version_recycling() {
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  mcas.control.fields.latest_slot = 1;

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
  mcas.control.fields.latest_slot = 2;

  noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  desc_read_slot = lock_descriptor_at(mcas.control, NOABA_VERSION_COUNT - 1);

  TEST_ASSERT(desc_read_slot.slot == 2 && desc_read_slot.lock == 0, "Did not recycle version lock (2), ctrl %lx",
              mcas.control.raw);
}

void test_noaba_cas_read_latest_slot_no_more_readers() {
  TestContentSlots content = { { 0 } };
  TestContent read_dst = { 0 };
  MCas_NoABA mcas = build_mcas_noaba(content);

  LockDesc fill_up_slot0 = {.slot = 0, .lock = NOABA_READERS - 1, .version = 0 };
  set_version_lock(&mcas.control, fill_up_slot0);

  MCAS_Stat stats = noaba_cas_read(&mcas, &read_dst, test_content_reader_func);
  TEST_ASSERT(!stats.success, "Cannot read when version lock is full");
  TEST_ASSERT(stats.acquire_tries == MAX_CAS_ATTEMPTS && stats.release_tries == 0 && stats.wait_spins > 0
                  && stats.version_overflow == MAX_CAS_ATTEMPTS && stats.all_ver_taken == 0,
              "Buggy stats acquisition");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.slot == 0, "An unsuccessful operation should not change anything");
    TEST_ASSERT((desc.version == fill_up_slot0.version && desc.lock == fill_up_slot0.lock) //
                    || (desc.version != fill_up_slot0.version && desc.lock == 0),          //
                "An unsuccessful operation should not change anything");
  }
}

void test_noaba_cas_read_all_version_slots_taken() {
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
  TEST_ASSERT(stats.acquire_tries == MAX_CAS_ATTEMPTS && stats.release_tries == 0 && stats.wait_spins > 0
                  && stats.version_overflow == 0 && stats.all_ver_taken == MAX_CAS_ATTEMPTS,
              "Buggy stats acquisition");

  FOREACH_VERSION_LOCK(mcas.control, desc) {
    TEST_ASSERT(desc.slot == 2 && desc.lock == 1, "An unsuccessful operation should not change anything");
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////

static void *noaba_cas_read_mt_helper(void *params_raw) {
  HelperParams *helper_params = (HelperParams *)params_raw;
  *(helper_params->stats) = noaba_cas_read(helper_params->mcas, helper_params->read_dst, test_content_reader_func);
  LOG_DEBUG("***");
  return params_raw;
}

void test_noaba_cas_read_simple_mt() {
  TestContentSlots content = { { 0 } };
  TestContent read_dst = {.x = 1, .y = 2, .z = 3 };
  MCas_NoABA mcas = build_mcas_noaba(content);
  MCAS_Stat stats = { 0 };
  HelperParams helper_params = {.mcas = &mcas, .stats = &stats, .read_dst = &read_dst };

  pthread_t reader_thr;
  int creation_ok = pthread_create(&reader_thr, NULL, noaba_cas_read_mt_helper, &helper_params);
  TEST_ASSERT(creation_ok == 0, "Failed to create reader thread");
  int join_ok = join_all(&reader_thr, 1, 500);
  TEST_ASSERT(join_ok == 0, "Reader thread never completed");
  TEST_ASSERT(read_dst.x == 0 && read_dst.y == 0 && read_dst.z == 0 && mcas.control.raw == 0 && stats.acquire_tries == 0
                  && stats.release_tries == 0,
              "Could not read from another thread, ctrl %lx", mcas.control.raw);
}
