#pragma once
#include "multi_swap_ctrl.h"
#include "test_common.h"

#include <stdint.h>

void print_stats_from_threads(MSWAP_Stat *, uint32_t);

void test_mask_calculation();
void test_lock_descriptor_at();
void test_set_version_lock();
void test_purge_all_version_locks();
void test_count_mswap_creation();

void test_increment_latest_reader_lock();
void test_lock_for_reading();
void test_lock_for_reading_failure();
void test_release_read_lock();
void test_release_read_lock_retry();

void test_count_mswap_read_simple();
void test_count_mswap_read_target_slot_already_readers();
void test_count_mswap_read_one_version_already_taken();
void test_count_mswap_read_version_recycling();
void test_count_mswap_read_latest_slot_no_more_readers();
void test_count_mswap_read_all_version_slots_taken();

void test_count_mswap_read_mt_simple();
void test_count_mswap_read_mt_many_simple();
void test_count_mswap_read_mt_many_with_wait();
void test_count_mswap_read_mt_slot_changing();
void test_count_mswap_read_mt_latest_slot_almost_full();
void test_count_mswap_read_mt_all_version_slots_taken();
void test_count_mswap_read_mt_contention_version_locks();

//////////////////////////////////////////////////////////////////////////////////////////////

static void test_count_utils() {
  test_mask_calculation();
  test_lock_descriptor_at();
  test_set_version_lock();
  test_purge_all_version_locks();
  test_count_mswap_creation();
}

static void test_count_single_thread_read() {
  test_increment_latest_reader_lock();
  test_lock_for_reading();
  test_lock_for_reading_failure();
  test_release_read_lock();
  test_release_read_lock_retry();

  test_count_mswap_read_simple();
  test_count_mswap_read_target_slot_already_readers();
  test_count_mswap_read_one_version_already_taken();
  test_count_mswap_read_version_recycling();
  test_count_mswap_read_latest_slot_no_more_readers();
  test_count_mswap_read_all_version_slots_taken();
}

static void test_count_multi_thread_read() {
  test_count_mswap_read_mt_simple();
  test_count_mswap_read_mt_many_with_wait();
  test_count_mswap_read_mt_many_simple();
  test_count_mswap_read_mt_slot_changing();
  test_count_mswap_read_mt_latest_slot_almost_full();
  test_count_mswap_read_mt_all_version_slots_taken();
  test_count_mswap_read_mt_contention_version_locks();
}

static void main_read_unit_test() {
  test_count_utils();
  test_count_single_thread_read();
  test_count_multi_thread_read();
}

