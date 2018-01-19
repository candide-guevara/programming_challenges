#include "1m_sorting.hpp"

using namespace std;

uint32_t Buckets::_r_extend(uint32_t target, uint32_t amount) {
  assert(amount);
  uint32_t prev_idx = prev_contiguous(target);
  if (prev_idx == Buckets::bucket_len)
    return make_ov_error(amount);
  auto prev_end_it = end(prev_idx);

  uint32_t avail = (prev_end_it.end - prev_end_it) / 8;
  if(avail < amount)
    return make_ov_error(amount - avail);
  ends[prev_idx] -= amount;

  auto read_it = begin(target);
  auto end_it = end(target);
  starts[target] -= amount;
  auto write_it = begin(target);
  int_len_t buffer = {0,0};

  while(read_it != end_it) {
    buffer = read_it.get_and_advance();
    write_it.write_and_advance(buffer.first);
  }
  update(target, write_it);
  return 0;
}


uint32_t Buckets::_swap(uint32_t from, uint32_t to) {
  assert(from != to);
  auto from_wr = begin(from), from_it = begin(from), from_end = end(from);
  auto to_wr = begin(to),     to_it = begin(to),     to_end = end(to);
  auto from_has_data = from_it != from_end;
  auto to_has_data   = to_it   != to_end;
  int_len_t from_buffer = {0,0}, to_buffer = {0,0};

  int32_t extra_len = std::max(
      (to_end - to_it) - (from_it.end - from_it),
      (from_end - from_it) - (to_it.end - to_it)
  );
  if(extra_len > 0)
    return make_ov_error(extra_len);

  do {
    from_has_data = from_it != from_end;
    to_has_data = to_it != to_end;

    if (from_has_data && from_buffer.second == 0) 
      from_buffer = from_it.get_and_advance();
    if (to_has_data && to_buffer.second == 0) 
      to_buffer = to_it.get_and_advance();

    if(from_buffer.second && (!to_has_data || from_buffer.second <= uint32_t(to_it - to_wr))) {
      auto ok = to_wr.write_and_advance(from_buffer.first);
      assert(!overflow_count(ok));
      from_buffer.second = 0;
    }
    if(to_buffer.second && (!from_has_data || to_buffer.second <= uint32_t(from_it - from_wr))) {
      auto ok = from_wr.write_and_advance(to_buffer.first);
      assert(!overflow_count(ok));
      to_buffer.second = 0;
    }
  }
  while(from_buffer.second || to_buffer.second || from_has_data || to_has_data);

  assert((to_wr - begin(to)) + (from_wr - begin(from)) == int32_t(lens[from] + lens[to]));
  update(from, from_wr);
  update(to, to_wr);
  return 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
void test_bucket_swap_same_len() {
  auto buckets = build_and_fill_first_bucket();

  DECLARE_INPUT;
  auto shuf_input = input;
  random_shuffle(shuf_input.begin(), shuf_input.end());
  fill_bucket(buckets, 3, shuf_input);

  auto ori_0_end = buckets.end(0);
  auto ori_3_end = buckets.end(3);

  buckets.swap(0, 3);
  assert(ori_0_end == buckets.end(0) && ori_3_end == buckets.end(3));
  compare_bucket_to_ref(shuf_input, buckets.at(0));
  compare_bucket_to_ref(input, buckets.at(3));

  buckets.swap(0, 3);
  assert(ori_0_end == buckets.end(0) && ori_3_end == buckets.end(3));
  compare_bucket_to_ref(shuf_input, buckets.at(3));
  compare_bucket_to_ref(input, buckets.at(0));
}

void test_bucket_swap_fuzzy() {
  auto buckets = build_buckets();

  for(uint32_t attempt = 0;
      attempt < 10000;
      ++attempt, buckets.clear()) {
    auto input1 = generate_rand_uint_input(0, bucket_max_value);
    auto input2 = generate_rand_uint_input(0, bucket_max_value);
    fill_bucket(buckets, 0, input1);
    fill_bucket(buckets, 1, input2);
    auto ori_0_len = buckets.lens[0];
    auto ori_1_len = buckets.lens[1];

    buckets.swap(0, 1);
    compare_bucket_to_ref(input1, buckets.at(1));
    compare_bucket_to_ref(input2, buckets.at(0));
    assert(ori_0_len == buckets.lens[1] && ori_1_len == buckets.lens[0]);

    buckets.swap(1, 0);
    compare_bucket_to_ref(input1, buckets.at(0));
    compare_bucket_to_ref(input2, buckets.at(1));
    assert(ori_0_len == buckets.lens[0] && ori_1_len == buckets.lens[1]);
  }
}

void test_bucket_swap_one_empty() {
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(1));
  assert(buckets.begin(0) == buckets.end(0));

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(0));
  assert(buckets.begin(1) == buckets.end(1));
}
*/

