#include <stdio.h>
#include <stdlib.h>

#include <common.h>
#include <logger.h>
#include <test.h>

void test_noaba_utils() {
  test_mask_calculation();
  test_lock_descriptor_at();
  test_set_version_lock();
  test_noaba_mcas_creation();
}

void test_noaba_single_thread_read() {
  TEST_ASSERT(NOABA_SLOTS > 4 && NOABA_VERSION_COUNT > 3 && NOABA_READERS > 4,
              "Compilation parameters do not match these tests");

  test_increment_latest_reader_lock();
  test_lock_for_reading();
  test_lock_for_reading_failure();
  test_release_read_lock();
  test_release_read_lock_retry();

  test_noaba_cas_read_simple();
  test_noaba_cas_read_target_slot_already_readers();
  test_noaba_cas_read_one_version_already_taken();
  test_noaba_cas_read_version_recycling();
  test_noaba_cas_read_latest_slot_no_more_readers();
  test_noaba_cas_read_all_version_slots_taken();
}

void test_noaba_multi_thread_read() { test_noaba_cas_read_simple_mt(); }


int main(void) {
  LOG_WARN("Starting ...");
  test_noaba_utils();
  test_noaba_single_thread_read();
  test_noaba_multi_thread_read();
  LOG_WARN("ALL DONE");
  return 0;
}
