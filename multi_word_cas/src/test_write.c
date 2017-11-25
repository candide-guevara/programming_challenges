#include <test_write.h>

#include <common.h>
#include <logger.h>
#include <multi_cas_noaba.h>
#include <util.h>

#include <stdatomic.h>
#include <string.h>

UNIT_TEST_START(test_magical_get_high_pos_bit_set)
  // edge case 0 => fails
  uint32_t input[] = { /*0,*/ -1, 1, 2, 3, 8, 9, 16, 20, 64, 127 };
  uint32_t expect[] = { /*0,*/ 31, 0, 1, 1, 3, 3, 4, 4, 6, 6 };

  for (uint32_t i = 0; i < (sizeof(input) / sizeof(uint32_t)); ++i) {
    uint32_t pos = magical_get_high_pos_bit_set(input[i]);
    TEST_ASSERT(pos == expect[i], "Failed to get high pos bit set : %d -> %d / %d", input[i], expect[i], pos);
  }
UNIT_TEST_END

UNIT_TEST_START(test_writer_mask_for_all_reading_slots)
#define TEST_ALL_VERSION_LOCKS(view_ptr, l0, r0, l1, r1, l2, r2, l3, r3)                                               \
  {                                                                                                                    \
    LockDesc d0 = {.slot = r0, .lock = l0, .version = 0 };                                                             \
    LockDesc d1 = {.slot = r1, .lock = l1, .version = 1 };                                                             \
    LockDesc d2 = {.slot = r2, .lock = l2, .version = 2 };                                                             \
    LockDesc d3 = {.slot = r3, .lock = l3, .version = 3 };                                                             \
    set_version_lock(view_ptr, d0);                                                                                    \
    set_version_lock(view_ptr, d1);                                                                                    \
    set_version_lock(view_ptr, d2);                                                                                    \
    set_version_lock(view_ptr, d3);                                                                                    \
    CtrlView_NoABA view = *(view_ptr);                                                                                 \
    uintmax_t mask = writer_mask_for_all_reading_slots(view);                                                          \
    TEST_ASSERT((mask & view.writer_mask) == 0, "We should ignore writer mask %lx / %lx", mask, view._raw);            \
    TEST_ASSERT(((mask >> r0) & 1) == l0, "Version lock 0 failed : %lx / %lx", mask, view._raw);                       \
    TEST_ASSERT(((mask >> r1) & 1) == l1, "Version lock 1 failed : %lx / %lx", mask, view._raw);                       \
    TEST_ASSERT(((mask >> r2) & 1) == l2, "Version lock 2 failed : %lx / %lx", mask, view._raw);                       \
    TEST_ASSERT(((mask >> r3) & 1) == l3, "Version lock 3 failed : %lx / %lx", mask, view._raw);                       \
  }
  // LOG_DEBUG("%lx => %lx", view._raw, mask);
  CtrlView_NoABA input[] = {
    { 0 }, { 0 }, { 0 }, {.writer_mask = 1 }, {.writer_mask = 3 },
  };
  TEST_ALL_VERSION_LOCKS(input + 0, 0, 0, 0, 0, 0, 0, 0, 0);
  TEST_ALL_VERSION_LOCKS(input + 1, 0, 1, 0, 2, 0, 3, 0, 4);
  TEST_ALL_VERSION_LOCKS(input + 2, 1, 1, 0, 2, 0, 3, 1, 4);
  TEST_ALL_VERSION_LOCKS(input + 3, 1, 5, 0, 2, 1, 3, 0, 4);
  TEST_ALL_VERSION_LOCKS(input + 4, 1, 2, 1, 3, 1, 4, 1, NOABA_LAST_SLOT);
UNIT_TEST_END

UNIT_TEST_START(test_get_all_bits_unset_positions_ordered)
  uintmax_t input[] = { 0x0, 0x1, 0xf, 0xff, 0xa5a, 0xfff, 0x5a5a, 0xffff, 0x3c3c3c, 0xffffff };
  uint32_t exp_8[] = { 8, 7, 4, 0, 4, 0, 4, 0, 4, 0 };
  uint32_t exp_16[] = { 16, 15, 12, 8, 10, 4, 8, 0, 8, 0 };
  uint32_t exp_20[] = { 20, 19, 16, 12, 14, 8, 12, 4, 10, 0 };

#define TEST_UNSET_BITS_FOR_LEN(bitlen, exp_len)                                                                       \
  for (uint32_t i = 0; i < (sizeof(input) / sizeof(uintmax_t)); ++i) {                                                 \
    uint8_t pos[8 * sizeof(uintmax_t)] = {};                                                                           \
    uint32_t len = get_all_bits_unset_positions_ordered(input[i], bitlen, pos);                                        \
    uintmax_t mask = (1 << bitlen) - 1;                                                                                \
    uintmax_t recons = (1 << bitlen) - 1;                                                                              \
    for (uint32_t j = 0; j < len; recons ^= (1llu << pos[j++]))                                                        \
      ;                                                                                                                \
    TEST_ASSERT(len == exp_len[i], "Failed to get unset bits %d: %lx / %d / %d", bitlen, input[i], len, exp_len[i]);   \
    TEST_ASSERT(recons == (mask & input[i]), "Failed to know positions %d: %lx / %lx", bitlen, input[i], recons);      \
  }
  // LOG_DEBUG("Calc %d: %lx / %lx / %d", bitlen, input[i], recons, len);

  TEST_UNSET_BITS_FOR_LEN(8, exp_8)
  TEST_UNSET_BITS_FOR_LEN(16, exp_16)
  TEST_UNSET_BITS_FOR_LEN(20, exp_20)
UNIT_TEST_END

UNIT_TEST_START(test_set_writer_mask_for_booked_slot)
  CtrlView_NoABA input[] = { { 0 },
                             {.writer_mask = 1, .latest_slot = 1 },
                             {.writer_mask = 2 },
                             {.writer_mask = 6 },
                             {.writer_mask = 0xff, .latest_slot = 8 },
                             {.writer_mask = NOABA_ALL_WRITERS - 3 },
                             {.writer_mask = NOABA_ALL_WRITERS - 0xf } };

  for (uint32_t i = 0; i < (sizeof(input) / sizeof(CtrlView_NoABA)); ++i) {
    MCAS_Stat stats = { 0 };
    LockResult result = set_writer_mask_for_booked_slot(input[i], &stats);

    // LOG_DEBUG("control %lx -> %lx", input[i]._raw, result.view._raw);
    TEST_ASSERT((result.view.writer_mask ^ input[i].writer_mask) & NOABA_ALL_WRITERS, //
                "Did not set writer lock on zeroed mcas %lx -> %lx", input[i]._raw, result.view._raw);
    TEST_ASSERT(stats.all_lock_taken == 0, "Buggy stats collection");
    TEST_ASSERT(result.success, "The operation should be successful");
  }
UNIT_TEST_END

UNIT_TEST_START(test_set_writer_mask_for_checking_readers)
  CtrlView_NoABA input[] = { {.writer_mask = NOABA_ALL_WRITERS - 7 }, //
                             {.writer_mask = NOABA_ALL_WRITERS - 7 } };
  SET_READ_LOCK_AND_SLOT(input, 1, 1, 0);
  SET_READ_LOCK_AND_SLOT(input, 1, 2, 1);
  SET_READ_LOCK_AND_SLOT(input + 1, 1, 1, 3);
  SET_READ_LOCK_AND_SLOT(input + 1, 1, 2, 1);

  for (uint32_t i = 0; i < (sizeof(input) / sizeof(CtrlView_NoABA)); ++i) {
    MCAS_Stat stats = { 0 };
    LockResult result = set_writer_mask_for_booked_slot(input[i], &stats);

    // LOG_DEBUG("control %lx -> %lx", input[i]._raw, result.view._raw);
    TEST_ASSERT(stats.all_lock_taken == 1, "Buggy stats collection");
    TEST_ASSERT(result.success == 0, "The operation should be failed");
  }
UNIT_TEST_END

UNIT_TEST_START(test_set_writer_mask_for_checking_latest)
  CtrlView_NoABA input[] = { {.writer_mask = NOABA_ALL_WRITERS - 1 }, //
                             {.writer_mask = NOABA_ALL_WRITERS - 8, .latest_slot = 3 } };

  for (uint32_t i = 0; i < (sizeof(input) / sizeof(CtrlView_NoABA)); ++i) {
    MCAS_Stat stats = { 0 };
    LockResult result = set_writer_mask_for_booked_slot(input[i], &stats);

    // LOG_DEBUG("control %lx -> %lx", input[i]._raw, result.view._raw);
    TEST_ASSERT(stats.all_lock_taken == 1, "Buggy stats collection");
    TEST_ASSERT(result.success == 0, "The operation should be failed");
  }
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_lock_for_writing_simple)
  MCas_NoABA mcas = {.control = {
                         .latest_slot = 3,
                     } };
  CtrlView_NoABA original = mcas.control;
  MCAS_Stat zero_stats = { 0 };

  for (uint32_t i = 0; i < NOABA_SLOTS - 1; ++i) {
    MCAS_Stat stats = { 0 };
    LockResult result = lock_for_read_or_write(&mcas, &stats, set_writer_mask_for_booked_slot, 0);
    CtrlView_NoABA mask = {.writer_mask = 1 << result.position };

    // LOG_DEBUG("i=%d, mask=%lx, view=%lx", i, mask._raw, mcas.control._raw);
    TEST_ASSERT(result.success, "Lock should have succeded");
    TEST_ASSERT(result.view.writer_mask & mask.writer_mask, //
                "Lock should have set %d bit in writer mask %lx / %lx", result.position, result.view._raw, mask._raw);
    TEST_ASSERT(result.view.writer_mask == mcas.control.writer_mask, "Result and mcas should be the same");
    TEST_ASSERT(result.view.latest_slot == original.latest_slot, "Latest slot should not have changed");
    TEST_ASSERT(result.view.version_locks == original.version_locks, "Version locks should not have changed");
    TEST_ASSERT(memcmp(&stats, &zero_stats, sizeof(MCAS_Stat)) == 0, "Buggy stats collection");

    mcas.control = result.view;
  }
UNIT_TEST_END

UNIT_TEST_START(test_lock_for_writing_fail_for_readers)
  MCas_NoABA mcas = {.control = {.writer_mask = NOABA_ALL_WRITERS - 0xf, .latest_slot = 2 } };
  MCAS_Stat stats = { 0 };
  uintmax_t start_mask = mcas.control.writer_mask;

  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 0, 3);
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 1, 2);
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 3, 0);
  LockResult result = lock_for_read_or_write(&mcas, &stats, set_writer_mask_for_booked_slot, 0);

  // LOG_DEBUG("mask=%lx, view=%lx, %lx", mcas.control._raw, result.view._raw, start_mask);
  TEST_ASSERT(!result.success, "Lock should have failed : %x", mcas.control.writer_mask);
  TEST_ASSERT(mcas.control.writer_mask == start_mask, "No mcas modification in failure");
  TEST_ASSERT(stats.all_lock_taken > 0 && stats.acquire_tries > 0, "Buggy stats collection");
UNIT_TEST_END

UNIT_TEST_START(test_lock_for_writing_fail_for_latest)
  MCas_NoABA mcas = {.control = {.writer_mask = NOABA_ALL_WRITERS - 4, .latest_slot = 2 } };
  MCAS_Stat stats = { 0 };
  uintmax_t start_mask = mcas.control.writer_mask;
  LockResult result = lock_for_read_or_write(&mcas, &stats, set_writer_mask_for_booked_slot, 0);

  // LOG_DEBUG("mask=%lx, view=%lx, %lx", mcas.control._raw, result.view._raw, start_mask);
  TEST_ASSERT(!result.success, "Lock should have failed : %x", mcas.control.writer_mask);
  TEST_ASSERT(mcas.control.writer_mask == start_mask, "No mcas modification in failure");
  TEST_ASSERT(stats.all_lock_taken > 0 && stats.acquire_tries > 0, "Buggy stats collection");
UNIT_TEST_END

UNIT_TEST_START(test_release_write_lock_simple)
  CtrlView_NoABA input[] = {
    {.writer_mask = 1 }, {.writer_mask = 2 }, {.writer_mask = NOABA_ALL_WRITERS }, {.writer_mask = NOABA_ALL_WRITERS }
  };
  uint32_t position[] = { 0, 1, 0, NOABA_LAST_SLOT };
  MCas_NoABA mcas = { 0 };
  MCAS_Stat zero_stats = { 0 };

  for (uint32_t i = 0; i < sizeof(input) / sizeof(CtrlView_NoABA); ++i) {
    MCAS_Stat stats = { 0 };
    CtrlView_NoABA mask = {.writer_mask = 1 << position[i] };
    mcas.control = input[i];
    bool result = release_write_lock(&mcas, input[i], position[i], &stats);

    // LOG_DEBUG("Test %d bit in writer mask %lx / %lx", position[i], mcas.control._raw, mask._raw);
    TEST_ASSERT((mcas.control.writer_mask & mask.writer_mask) == 0 //
                    && input[i].writer_mask & mask.writer_mask,    //
                "Lock should have set %d bit in writer mask %lx / %lx", position[i], mcas.control._raw, mask._raw);
    TEST_ASSERT(mcas.control.version_locks == 0, "Version locks should not have been touched");
    TEST_ASSERT(mcas.control.latest_slot == position[i], "Failed to set new latest slot");
    TEST_ASSERT(result, "Release should have succeded");
    TEST_ASSERT(memcmp(&stats, &zero_stats, sizeof(MCAS_Stat)) == 0, "Buggy stats collection");
  }
UNIT_TEST_END

UNIT_TEST_START(test_release_write_lock_changed)
  MCas_NoABA mcas = {.control = {.writer_mask = 3 } };
  CtrlView_NoABA previous = {.writer_mask = 2 };
  MCAS_Stat stats = { 0 };

  bool result = release_write_lock(&mcas, previous, 1, &stats);
  TEST_ASSERT(result, "Release should have succeded");
  TEST_ASSERT(mcas.control.writer_mask == 1, "Did not take into account latest mcas change");
  TEST_ASSERT(mcas.control.version_locks == 0, "Version locks should not have been touched");
  TEST_ASSERT(mcas.control.latest_slot == 1, "Failed to set new latest slot");
  TEST_ASSERT(stats.release_tries <= 1, "Depending on compilation options we may retry once");
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_noaba_cas_write_simple)
  TestContentSlots content = { 0 };
  TestContent write_src = {.x = 111, .y = 222, .z = 333 };
  MCas_NoABA mcas = {.control = { 0 }, .content = &content };
  MCAS_Stat expect_stats = {.success = 1 };

  for (uint32_t i = 0; i < NOABA_SLOTS * 2; ++i) {
    write_src.x += i;
    uintmax_t previous = mcas.control.latest_slot;
    MCAS_Stat stats = noaba_cas_write(&mcas, &write_src, test_content_writer_func);
    TestContent *write_dst = (TestContent *)mcas.content + mcas.control.latest_slot;

    // char buffer[4096];
    // print_mem(mcas.content, sizeof(TestContentSlots), buffer);
    // LOG_DEBUG("TestContentSlots : %s", buffer);
    // uint32_t written = print_mem(&write_src, sizeof(TestContent), buffer);
    // print_mem(write_dst, sizeof(TestContent), buffer + written + 1);
    // LOG_DEBUG("\nsrc: %s\ndst: %s", buffer, buffer + written + 1);

    TEST_ASSERT(memcmp(&stats, &expect_stats, sizeof(MCAS_Stat)) == 0, "Buggy stats collection");
    TEST_ASSERT(mcas.control.latest_slot != previous, "latest_slot should change in each write");
    TEST_ASSERT(memcmp(write_dst, &write_src, sizeof(TestContent)) == 0, "Failed to write to %u",
                mcas.control.latest_slot);
  }
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_all_slots_taken)
  TestContentSlots content = { 0 };
  TestContent write_src = {.x = 111, .y = 222, .z = 333 };
  MCas_NoABA mcas = {.control = {.writer_mask = NOABA_ALL_WRITERS - 1 }, .content = &content };

  MCAS_Stat stats = noaba_cas_write(&mcas, &write_src, test_content_writer_func);
  TestContent *write_dst = (TestContent *)mcas.content + mcas.control.latest_slot;

  TEST_ASSERT(stats.success == 0, "Operation should fail");
  TEST_ASSERT(mcas.control.latest_slot == 0, "Latest slot should stay the same");
  TEST_ASSERT(stats.all_lock_taken > 0 && stats.version_overflow == 0, "Operation should fail");
  TEST_ASSERT(memcmp(write_dst, &write_src, sizeof(TestContent)) != 0, "Nothing should be written to %u",
              mcas.control.latest_slot);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_only_slot_has_reader)
  TestContentSlots content = { 0 };
  TestContent write_src = {.x = 111, .y = 222, .z = 333 };
  MCas_NoABA mcas = {.control = {.writer_mask = NOABA_ALL_WRITERS - 0xf }, .content = &content };
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 0, 3);
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 1, 2);
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 2, 1);
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 3, 0);

  MCAS_Stat stats = noaba_cas_write(&mcas, &write_src, test_content_writer_func);
  TEST_ASSERT(stats.success == 0, "Operation should fail");
  TEST_ASSERT(stats.all_lock_taken > 0 && stats.version_overflow == 0, "Operation should fail");
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_avoid_read_clobber)
  TestContentSlots content = { 0 };
  TestContent write_src = {.x = 111, .y = 222, .z = 333 };
  MCas_NoABA mcas = {.control = {.writer_mask = NOABA_ALL_WRITERS - 3, .latest_slot = 1 }, .content = &content };
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 1, 2);

  MCAS_Stat stats_ok = noaba_cas_write(&mcas, &write_src, test_content_writer_func);
  TEST_ASSERT(stats_ok.success == 1, "Operation should succeed");
  TEST_ASSERT(mcas.control.latest_slot == 0, "Latest slot should move to only free slot");
  MCAS_Stat stats_ko = noaba_cas_write(&mcas, &write_src, test_content_writer_func);
  TEST_ASSERT(stats_ko.success == 0, "Operation should fail since free slots exhausted");
  TEST_ASSERT(mcas.control.latest_slot == 0, "Latest slot should stay the same");
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

static void *mcas_write_in_loop(void *params_raw) {
  HelperParams *params = (HelperParams *)params_raw;
  while (params->thr_iterations--) {
    MCAS_Stat stats = noaba_cas_write(params->mcas, params->target, test_content_writer_func);
    mcas_stats_merge(params->stats, &stats);
    if (stats.success && params->wait_for)
      busy_spin_for(params->wait_for);
  }
  return params_raw;
}

static void *mcas_read_in_loop(void *params_raw) {
  HelperParams *params = (HelperParams *)params_raw;
  while (params->thr_iterations--) {
    MCAS_Stat stats = noaba_cas_read(params->mcas, params->target, test_content_reader_func);
    mcas_stats_merge(params->stats, &stats);
    if (stats.success && params->wait_for)
      busy_spin_for(params->wait_for);
  }
  return params_raw;
}

static void *mcas_fake_reader(void *params_raw) {
  HelperParams *params = (HelperParams *)params_raw;
  CtrlView_NoABA *view = &params->mcas->control;
  CtrlView_NoABA previous = params->mcas->control;

  while (pthread_mutex_trylock(&params->stop)) {
    uintmax_t free_mask = ~(1 << previous.latest_slot | previous.writer_mask) & NOABA_ALL_WRITERS;

    if (free_mask == 0) {
      params->stats->all_lock_taken += 1;
      params->stats->acquire_tries += 1;
      if (params->wait_for)
        busy_spin_for(params->wait_for);
      continue;
    }

    uint32_t position = magical_get_high_pos_bit_set(free_mask);
    CtrlView_NoABA next = previous;
    SET_READ_LOCK_AND_SLOT(&next, 1, position, position % NOABA_VERSION_COUNT);
    uint32_t swap_ok = atomic_compare_exchange_strong(&view->raw, &previous._raw, next._raw);

    if (swap_ok) {
      params->stats->success += 1;
      if (params->wait_for) {
        busy_spin_for(params->wait_for);
        purge_all_version_locks(&(params->mcas->control));
      }
    }
    else
      params->stats->acquire_tries += 1;
  }
  return params_raw;
}

UNIT_TEST_START(test_noaba_cas_write_mt_simple)
  MT_MCAS_BOILER_PLATE(1, 10, mcas, write_src, stats, params);
  MT_KICKOFF_MCAS_THREADS(writer_thr, write_src, stats, params, mcas_write_in_loop);
  MT_JOIN_MCAS_THREADS(writer_thr, write_src, stats, params);

  TestContent *last_dst = (TestContent *)(mcas.content) + mcas.control.latest_slot;
  TEST_ASSERT(params[0].stats->success == 10, "All writes should succeed");
  TEST_ASSERT(memcmp(last_dst, &write_src, sizeof(TestContent)) == 0, "Failed to write into %u",
              mcas.control.latest_slot);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_many)
  MT_MCAS_BOILER_PLATE(4, 1000, mcas, write_src, stats, params);
  MT_KICKOFF_MCAS_THREADS(writer_thr, write_src, stats, params, mcas_write_in_loop);
  MT_JOIN_MCAS_THREADS(writer_thr, write_src, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_many_with_wait)
  MT_MCAS_BOILER_PLATE(6, 10000, mcas, write_src, stats, params);
  for (uint32_t i = 0; i < max_mcas_thr; ++i)
    params[i].wait_for = (WAIT_QUANTUM + i) * 10;

  MT_KICKOFF_MCAS_THREADS(writer_thr, write_src, stats, params, mcas_write_in_loop);
  MT_JOIN_MCAS_THREADS(writer_thr, write_src, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_few_write_slots)
  MT_MCAS_BOILER_PLATE(5, 1000, mcas, write_src, stats, params);
  // we leave only 2 slots free
  mcas.control.writer_mask = NOABA_ALL_WRITERS - 0xf;
  mcas.control.latest_slot = 3;
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 2, 1);
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 3, 0);

  MT_KICKOFF_MCAS_THREADS(writer_thr, write_src, stats, params, mcas_write_in_loop);
  MT_JOIN_MCAS_THREADS(writer_thr, write_src, stats, params);

  uint32_t tot_all_lock_taken = 0, tot_acquire_tries = 0;
  FOREACH_MCAS_THREAD(thr, write_src, stats, params) {
    tot_all_lock_taken += params_i->stats->all_lock_taken;
    tot_acquire_tries += params_i->stats->acquire_tries;
  }
  TEST_ASSERT(tot_acquire_tries > 0 && tot_acquire_tries >= tot_all_lock_taken, "Buggy stat collection");
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_fake_reader)
  MT_MCAS_BOILER_PLATE(4, 10000, mcas, write_src, stats, params);
  mcas.control.writer_mask = NOABA_ALL_WRITERS - 0xf;
  MCAS_Stat fake_stats = { 0 };
  HelperParams fake_params = {
    .mcas = &mcas, .stats = &fake_stats, .wait_for = WAIT_QUANTUM * 2, .stop = PTHREAD_MUTEX_INITIALIZER,
  };

  pthread_t fake_thr;
  pthread_mutex_trylock(&fake_params.stop);
  int create_ok = pthread_create(&fake_thr, NULL, mcas_fake_reader, &fake_params);
  TEST_ASSERT(create_ok == 0, "Could not create fake reader");

  MT_KICKOFF_MCAS_THREADS(writer_thr, write_src, stats, params, mcas_write_in_loop);
  MT_JOIN_MCAS_THREADS(writer_thr, write_src, stats, params);

  pthread_mutex_unlock(&fake_params.stop);
  int join_ok = join_all(&fake_thr, 1, 666);
  char buffer[1024];
  mcas_stats_print(buffer, fake_stats);
  TEST_ASSERT(join_ok == 0, "Could not join fake reader after releasing lock");
  TEST_ASSERT(fake_stats.success > 0, "Fake reader did not lock anything : %s", buffer);
  TEST_ASSERT(fake_stats.acquire_tries > 0, "Fake reader did cause any contention : %s", buffer);
UNIT_TEST_END

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_noaba_cas_write_mt_one_reader_one_writer)
  MT_MCAS_BOILER_PLATE(2, 100000, mcas, src_dst, stats, params);
  MT_KICKOFF_WRITE_READ_THR(1, mcas_thr, src_dst, stats, params, mcas_write_in_loop, mcas_read_in_loop);
  MT_JOIN_MCAS_THREADS(mcas_thr, src_dst, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_many_reader_many_writer)
  MT_MCAS_BOILER_PLATE(6, 100000, mcas, src_dst, stats, params);
  MT_KICKOFF_WRITE_READ_THR(3, mcas_thr, src_dst, stats, params, mcas_write_in_loop, mcas_read_in_loop);
  MT_JOIN_MCAS_THREADS(mcas_thr, src_dst, stats, params);
  MT_ASSERT_CONTENTION(src_dst, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_readers_writers_wait)
  MT_MCAS_BOILER_PLATE(6, 100000, mcas, src_dst, stats, params);
  for (uint32_t i = 0; i < 3; ++i)
      params[i].wait_for = (WAIT_QUANTUM + i) * 3;
  MT_KICKOFF_WRITE_READ_THR(3, mcas_thr, src_dst, stats, params, mcas_write_in_loop, mcas_read_in_loop);
  MT_JOIN_MCAS_THREADS(mcas_thr, src_dst, stats, params);
  MT_ASSERT_CONTENTION(src_dst, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_more_reader_than_writer)
  MT_MCAS_BOILER_PLATE(5, 100000, mcas, src_dst, stats, params);
  MT_KICKOFF_WRITE_READ_THR(1, mcas_thr, src_dst, stats, params, mcas_write_in_loop, mcas_read_in_loop);
  MT_JOIN_MCAS_THREADS(mcas_thr, src_dst, stats, params);
  MT_ASSERT_CONTENTION(src_dst, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_more_writer_than_reader)
  MT_MCAS_BOILER_PLATE(5, 100000, mcas, src_dst, stats, params);
  MT_KICKOFF_WRITE_READ_THR(4, mcas_thr, src_dst, stats, params, mcas_write_in_loop, mcas_read_in_loop);
  MT_JOIN_MCAS_THREADS(mcas_thr, src_dst, stats, params);
  MT_ASSERT_CONTENTION(src_dst, stats, params);
UNIT_TEST_END

UNIT_TEST_START(test_noaba_cas_write_mt_read_write_contention)
  MT_MCAS_BOILER_PLATE(5, 100000, mcas, src_dst, stats, params);
  mcas.control.writer_mask = NOABA_ALL_WRITERS - 0x3f;
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 4, 0);
  SET_READ_LOCK_AND_SLOT(&mcas.control, 1, 5, 3);
  MT_KICKOFF_WRITE_READ_THR(2, mcas_thr, src_dst, stats, params, mcas_write_in_loop, mcas_read_in_loop);
  MT_JOIN_MCAS_THREADS(mcas_thr, src_dst, stats, params);
  MT_ASSERT_CONTENTION(src_dst, stats, params);

  uint32_t all_lock_taken = 0;
  FOREACH_MCAS_THREAD(thr, src_dst, stats, params) { all_lock_taken += stats_i->all_lock_taken; }
  TEST_ASSERT(all_lock_taken > 0, "Read version locks or writer locks should have been contended");
UNIT_TEST_END
