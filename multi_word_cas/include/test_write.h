#pragma once
#include "multi_cas_ctrl.h"
#include "test_common.h"

#include <stdint.h>

void test_magical_get_high_pos_bit_set();
void test_writer_mask_for_all_reading_slots();
void test_get_all_bits_unset_positions_ordered();
void test_set_writer_mask_for_booked_slot();
void test_set_writer_mask_for_checking_readers();
void test_set_writer_mask_for_checking_latest();
void test_lock_for_writing_simple();
void test_lock_for_writing_fail_for_readers();
void test_lock_for_writing_fail_for_latest();
void test_release_write_lock_simple();
void test_release_write_lock_changed();
void test_noaba_cas_write_simple();
void test_noaba_cas_write_all_slots_taken();
void test_noaba_cas_write_only_slot_has_reader();
void test_noaba_cas_write_avoid_read_clobber();
void test_noaba_cas_write_mt_simple();
void test_noaba_cas_write_mt_many_with_wait();
void test_noaba_cas_write_mt_many();
void test_noaba_cas_write_mt_few_write_slots();
void test_noaba_cas_write_mt_fake_reader();
void test_noaba_cas_write_mt_one_reader_one_writer();
void test_noaba_cas_write_mt_many_reader_many_writer();
void test_noaba_cas_write_mt_readers_writers_wait();
void test_noaba_cas_write_mt_more_reader_than_writer();
void test_noaba_cas_write_mt_more_writer_than_reader();
void test_noaba_cas_write_mt_read_write_contention();

//////////////////////////////////////////////////////////////////////////////////////////////

static void test_noaba_write_utils() {
  test_magical_get_high_pos_bit_set();
  test_writer_mask_for_all_reading_slots();
  test_get_all_bits_unset_positions_ordered();
  test_set_writer_mask_for_booked_slot();
  test_set_writer_mask_for_checking_readers();
  test_set_writer_mask_for_checking_latest();
}

static void test_noaba_single_thread_write() {
  test_lock_for_writing_simple();
  test_lock_for_writing_fail_for_readers();
  test_lock_for_writing_fail_for_latest();
  test_release_write_lock_simple();
  test_release_write_lock_changed();
  test_noaba_cas_write_simple();
  test_noaba_cas_write_all_slots_taken();
  test_noaba_cas_write_only_slot_has_reader();
  test_noaba_cas_write_avoid_read_clobber();
}

static void test_noaba_multi_thread_write() {
  test_noaba_cas_write_mt_simple();
  test_noaba_cas_write_mt_many_with_wait();
  test_noaba_cas_write_mt_many();
  test_noaba_cas_write_mt_few_write_slots();
  test_noaba_cas_write_mt_fake_reader();
  test_noaba_cas_write_mt_one_reader_one_writer();
  test_noaba_cas_write_mt_many_reader_many_writer();
  test_noaba_cas_write_mt_readers_writers_wait();
  test_noaba_cas_write_mt_more_reader_than_writer();
  // fails in debug mode (too slow on reads ...)
  test_noaba_cas_write_mt_read_write_contention();
  test_noaba_cas_write_mt_more_writer_than_reader();
}

static void main_write_unit_test() {
  test_noaba_write_utils();
  test_noaba_single_thread_write();
  test_noaba_multi_thread_write();
}
