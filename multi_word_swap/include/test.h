#pragma once
#include "multi_cas_ctrl.h"

#include <stdint.h>

typedef struct TestContent TestContent;
typedef struct HelperParams HelperParams;

struct TestContent {
  uint64_t x, y, z;
} __attribute__((aligned(64)));

struct HelperParams {
  MCas_NoABA *mcas;
  MCAS_Stat *stats;
  TestContent *read_dst;
};

typedef TestContent TestContentSlots[NOABA_SLOTS];

void test_content_writer_func(void *, uint32_t, void *);
void test_content_reader_func(void *, uint32_t, void *);

void test_mask_calculation();
void test_lock_descriptor_at();
void test_set_version_lock();
void test_noaba_mcas_creation();

void test_increment_latest_reader_lock();
void test_lock_for_reading();
void test_lock_for_reading_failure();
void test_release_read_lock();
void test_release_read_lock_retry();

void test_noaba_cas_read_simple();
void test_noaba_cas_read_target_slot_already_readers();
void test_noaba_cas_read_one_version_already_taken();
void test_noaba_cas_read_version_recycling();
void test_noaba_cas_read_latest_slot_no_more_readers();
void test_noaba_cas_read_all_version_slots_taken();

void test_noaba_cas_read_simple_mt();
