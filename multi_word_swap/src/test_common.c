#include <test_common.h>

#include <common.h>
#include <logger.h>
#include <multi_swap_count.h>

void test_content_reader_func(void *restrict src, uint32_t slot, void *restrict dst) {
  TestContent *src_content = (TestContent *)src + slot;
  TestContent *dst_content = (TestContent *)dst;
  dst_content->x = src_content->x;
  dst_content->y = src_content->y;
  dst_content->z = src_content->z;
}

void test_content_writer_func(void *restrict src, uint32_t slot, void *restrict dst) {
  TestContent *src_content = (TestContent *)src + slot;
  TestContent *dst_content = (TestContent *)dst;
  src_content->x = dst_content->x;
  src_content->y = dst_content->y;
  src_content->z = dst_content->z;
}

void test_lock_reader_func(void *restrict src, void *restrict dst) {
  TestContent *src_content = (TestContent *)src;
  TestContent *dst_content = (TestContent *)dst;
  dst_content->x = src_content->x;
  dst_content->y = src_content->y;
  dst_content->z = src_content->z;
}

void test_lock_writer_func(void *restrict src, void *restrict dst) {
  test_lock_reader_func(dst, src);
}

void print_stats_from_threads(MSWAP_Stat *stats, uint32_t thr_count) {
  for (uint32_t thr = 0; thr < thr_count; ++thr) {
    char stat_buf[256];
    mswap_stats_print(stat_buf, stats[thr]);
    LOG_WARN("%d : %s", thr, stat_buf);
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////

UNIT_TEST_START(test_count_mswap_parameters)
  TEST_ASSERT(SWCOUNT_SLOTS == 16 && SWCOUNT_READERS == 128 && SWCOUNT_VERSION_COUNT == 4,
              "Compilation parameters do not match test suite");
UNIT_TEST_END

UNIT_TEST_START(test_count_mswap_alignment)
  ALIGN_CACHE_LINE MSWAP_Count mswap = { 0 };
  uint32_t alignment = (size_t)&mswap % CACHE_LINE_SIZE;
  TEST_ASSERT(alignment == 0, "align constraint not taken into account mswap");
  alignment = (size_t)&mswap.content % CACHE_LINE_SIZE;
  TEST_ASSERT(alignment != 0, "align constraint not taken into account mswap.content");
  alignment = (size_t)&mswap.control % CACHE_LINE_SIZE;
  TEST_ASSERT(alignment == 0, "align constraint not taken into account mswap.control");
UNIT_TEST_END

// Portability issues with bit fields
// https://stackoverflow.com/questions/6043483/why-bit-endianness-is-an-issue-in-bitfields
UNIT_TEST_START(test_bit_field_mem_layout)
  CtrlView_Count layout_test_1 = {.latest_slot = 1 };
  TEST_ASSERT(layout_test_1._raw | 1u, "layout_test_1");
  CtrlView_Count layout_test_2 = {.version_locks = 1 };
  TEST_ASSERT(layout_test_2._raw | 1u << (SWCOUNT_SLOT_EXP + SWCOUNT_SLOTS), "layout_test_2");
  CtrlView_Count layout_test_3 = {.writer_mask = 1 };
  TEST_ASSERT(layout_test_3._raw | 1u << SWCOUNT_SLOT_EXP, "layout_test_3");

  CtrlView_Count layout_test_4 = {.latest_slot = 2 };
  TEST_ASSERT(layout_test_4._raw | 1u << 1, "layout_test_4");
  CtrlView_Count layout_test_5 = {.version_locks = 2 };
  TEST_ASSERT(layout_test_5._raw | 1u << (1 + SWCOUNT_SLOT_EXP + SWCOUNT_SLOTS), "layout_test_5");
  CtrlView_Count layout_test_6 = {.writer_mask = 2 };
  TEST_ASSERT(layout_test_6._raw | 1u << (1 + SWCOUNT_SLOT_EXP), "layout_test_6");
UNIT_TEST_END

IGNORE_WARNING_PUSH("-Woverflow")
#ifdef __clang__
IGNORE_WARNING_PUSH("-Wbitfield-constant-conversion")
#endif
UNIT_TEST_START(test_bit_field_overflow)
  CtrlView_Count ov_test_1 = {.latest_slot = SWCOUNT_SLOTS };
  TEST_ASSERT(ov_test_1.latest_slot == 0 && ov_test_1._raw == 0,
              "Value greater than bit fields overflow to next bit field : %lx", ov_test_1._raw);
  CtrlView_Count ov_test_2 = {.writer_mask = SWCOUNT_ALL_WRITERS + 3 };
  TEST_ASSERT(ov_test_2.writer_mask == 2 && ov_test_1.version_locks == 0,
              "Value greater than bit fields overflow to next bit field : %lx", ov_test_2._raw);
  CtrlView_Count ov_test_3 = {.latest_slot = SWCOUNT_SLOTS + SWCOUNT_LAST_SLOT };
  TEST_ASSERT(ov_test_3.latest_slot == SWCOUNT_LAST_SLOT, //
              "Value greater than bit fields overflow to next bit field : %lx", ov_test_3._raw);
UNIT_TEST_END
#ifdef __clang__
IGNORE_WARNING_POP
#endif
IGNORE_WARNING_POP

//////////////////////////////////////////////////////////////////////////////////////////////

void main_common_unit_test() {
  test_count_mswap_parameters();
  test_count_mswap_alignment();
  test_bit_field_mem_layout();
  test_bit_field_overflow();
}

//////////////////////////////////////////////////////////////////////////////////////////////