#include "1m_sorting.hpp"
#include "1m_sorting_print.hpp"
#include <algorithm>
#include <functional>
#include <random>

#define DECLARE_INPUT \
  vector<uint32_t> input = { 0, 1, 32, 64, 90, 100, 127, 128, \
                            comp_max_1-1, comp_max_1, comp_max_1+1, \
                            comp_max_2-1, comp_max_2, comp_max_2+1, \
                            comp_max_3-1, comp_max_3, comp_max_3+1, \
                            max_v_mask-1, max_v_mask }

using namespace std;

void compare_bucket_to_ref(const vector<uint32_t>& ref_vect, ItContainer<BucketIt> bucket) {
  uint32_t idx = 0;
  for(auto value : bucket) {
    auto ref = ref_vect[idx++];
    if (ref != value) {
      LOG("mismatch at " << idx << " : " << ref << " != " << value);
      MY_ASSERT(ref == value);
    }
  }
}

void compare_deltas_to_ref(const vector<uint32_t>& ref_vect, ItContainer<BucketIt> bucket) {
  uint32_t idx = 0, sum = 0;
  for(auto n : bucket) {
    auto ref = ref_vect[idx++];
    sum += n;
    if(ref != sum) {
      LOG("mismatch at " << n << " : " << sum << " != " << ref);
      MY_ASSERT(ref == sum);
    }
  }
}

void fill_bucket_to_max(Buckets& buckets, uint32_t target, uint32_t value) {
  auto write_it = buckets.at(target).begin();
  uint32_t ok = 0;
  while(!overflow_count(ok)) 
    ok = write_it.write_and_advance(value);
  buckets.update(target, write_it);
  MY_ASSERT(buckets.available(target) < comp_len_4);
}

void fill_bucket(Buckets& buckets, uint32_t target, const vector<uint32_t>& ref_vect) {
  auto write_it = buckets.at(target).begin();
  auto idx = 0;
  for(auto u : ref_vect) {
    uint32_t result = write_it.write_and_advance(u);
    idx ++;
    MY_ASSERT(!overflow_count(result));
  }
  buckets.update(target, write_it);
}

void fill_bucket_raw(Buckets& buckets, uint32_t target, const vector<uint8_t>& ref_vect) {
  auto start = buckets.starts[target];
  auto start_it = buckets.begin(target);
  std::copy(ref_vect.begin(), ref_vect.end(), start);
  start_it.shift_bits(8 * ref_vect.size());
  buckets.update(target, start_it);
}

void clean_bucket_raw(Buckets& buckets, uint32_t target, uint8_t value=0) {
  std::fill(buckets.starts[target], buckets.ends[target], value);
  buckets.lens[target] = 0;
}

vector<uint32_t> generate_rand_uint_input(uint32_t len, uint32_t max_item_val=max_v_mask) {
  random_device rd;
  mt19937 gen(rd());
  uniform_int_distribution<> dist(0, max_item_val);

  if(len == 0) {
    uint32_t width_bit = compress_len(max_item_val);
    uint32_t width = (width_bit % 8) ? width_bit/8 + 1 : width_bit/8;
    uint32_t max_to_fit = buffer_len / (bucket_len * width) - 1;
    len = dist(gen) % max_to_fit;
  }
  vector<uint32_t> input;
  input.reserve(len);
  for(uint32_t i=0; i<len; ++i)
    input.push_back(dist(gen));
  return input;
}

array<uint8_t, 1024> write_compressed_data() {
  DECLARE_INPUT;
  array<uint8_t, 1024> data;
  uint8_t* start=data.data(), *end=data.data()+data.size();
  uint32_t bit_offset = 0;

  for(auto u : input) {
    bit_offset += write_compressed(u, start, bit_offset, end);
    start += bit_offset / 8;
    bit_offset = bit_offset % 8;
  }
  return data;
}

Buckets build_and_fill_first_bucket() {
  DECLARE_INPUT;
  auto buckets = build_buckets();
  fill_bucket(buckets, 0, input);
  return buckets;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void test_compression() {
  TEST_HEADER();
  DECLARE_INPUT;
  for(auto u : input) {
    auto comp = compress(u);
    auto decomp = decompress(comp);
    LOG(print_collection(comp) << " " << u << " " << decomp);
  }
}

void test_compression_exhaustive() {
  TEST_HEADER();
  for(uint32_t u=0; u<max_v_mask; ++u) {
    auto comp = compress(u);
    auto decomp = decompress(comp);
    if (u != decomp) {
      LOG(print_collection(comp) << " " << u << " " << decomp);
      MY_ASSERT(u == decomp);
    }
  }
}

void test_bucket_iteration() {
  TEST_HEADER();
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();
  compare_bucket_to_ref(input, buckets.at(0));
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void test_compress_len() {
  TEST_HEADER();
  for(uint32_t number=0; number<comp_max_1; ++number)
    MY_ASSERT(compress_len(number) == comp_len_1);

  for(uint32_t number=comp_max_1; number<comp_max_2; ++number)
    MY_ASSERT(compress_len(number) == comp_len_2);

  for(auto n : generate_rand_uint_input(1000000, comp_max_3 - comp_max_2 - 1))
    MY_ASSERT(compress_len(comp_max_2 + n) == comp_len_3);

  MY_ASSERT(compress_len(comp_max_3 - 1) == comp_len_3);
  MY_ASSERT(compress_len(comp_max_3) == comp_len_4);
}

void test_bucket_algorithm() {
  TEST_HEADER();
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();
  vector<uint32_t> to_find = {100, 0, 1, max_v_mask};
  vector<uint32_t> to_miss = {333, 666, 289347};

  for(uint32_t u : to_find) {
    auto it = find(buckets.begin(0), buckets.end(0), u);
    if(it == buckets.end(0)) {
      LOG(u << " " << distance(it.start, buckets.starts[0]));
      MY_ASSERT(it != buckets.end(0));
    }
  }
  for(uint32_t u : to_miss) {
    auto it = find(buckets.begin(0), buckets.end(0), u);
    if(it != buckets.end(0)) {
      LOG(u << " " << distance(it.start, buckets.starts[0]));
      MY_ASSERT(it == buckets.end(0));
    }
  }

  auto sum_it = accumulate(buckets.begin(0), buckets.end(0), 0);
  auto sum_ref = accumulate(input.begin(), input.end(), 0);
  MY_ASSERT(sum_it == sum_ref);
}

void test_codec_fuzzy() {
  TEST_HEADER();
  uint32_t idx = 0;
  auto numbers = generate_rand_uint_input(input_len, comp_max_1-1);
  auto number2 = generate_rand_uint_input(input_len, comp_max_2);
  auto number3 = generate_rand_uint_input(input_len, comp_max_3);
  auto number4 = generate_rand_uint_input(input_len);
  numbers.insert(numbers.end(), number2.begin(), number2.end());
  numbers.insert(numbers.end(), number3.begin(), number3.end());
  numbers.insert(numbers.end(), number4.begin(), number4.end());
  std::random_shuffle(numbers.begin(), numbers.end());

  vector<uint8_t> buffer;
  buffer.resize(numbers.size() * comp_len_4/8);
  BucketIt write_it {buffer.data(), buffer.data() + buffer.size(), 0};
  BucketIt start_it {buffer.data(), buffer.data() + buffer.size(), 0};

  for(auto u : numbers)
    write_it.write_and_advance(u);

  ItContainer<BucketIt> container {start_it, write_it};
  for(auto value : container) { 
    if(numbers[idx] != value) {
      LOG("at " << idx << " : " << numbers[idx] << " != " << value);
      MY_ASSERT(numbers[idx] == value);
    }
    idx += 1;
  }
}

void test_bucket_swap_same_len() {
  TEST_HEADER();
  auto buckets = build_and_fill_first_bucket();

  DECLARE_INPUT;
  auto shuf_input = input;
  random_shuffle(shuf_input.begin(), shuf_input.end());
  fill_bucket(buckets, 3, shuf_input);

  auto ori_0_end = buckets.end(0);
  auto ori_3_end = buckets.end(3);

  buckets.swap(0, 3);
  MY_ASSERT(ori_0_end == buckets.end(3) && ori_3_end == buckets.end(0));
  compare_bucket_to_ref(shuf_input, buckets.at(3));
  compare_bucket_to_ref(input, buckets.at(0));

  buckets.swap(0, 3);
  MY_ASSERT(ori_0_end == buckets.end(0) && ori_3_end == buckets.end(3));
  compare_bucket_to_ref(shuf_input, buckets.at(3));
  compare_bucket_to_ref(input, buckets.at(0));
}

void test_bucket_swap_fuzzy() {
  TEST_HEADER();
  auto buckets = build_buckets();

  for(uint32_t attempt = 0;
      attempt < 10000;
      ++attempt, buckets.clear()) {
    auto input1 = generate_rand_uint_input(0, bucket_max_value);
    auto input2 = generate_rand_uint_input(0, bucket_max_value);
    fill_bucket(buckets, 0, input1);
    fill_bucket(buckets, 1, input2);
    uint32_t ori_0_len = buckets.lens[0];
    uint32_t ori_1_len = buckets.lens[1];
    uint8_t *ori_0_start = buckets.starts[0];
    uint8_t *ori_1_start = buckets.starts[1];
    uint8_t *ori_0_end = buckets.ends[0];
    uint8_t *ori_1_end = buckets.ends[1];

    buckets.swap(0, 1);
    compare_bucket_to_ref(input1, buckets.at(0));
    compare_bucket_to_ref(input2, buckets.at(1));
    MY_ASSERT(ori_0_end == buckets.ends[1] && ori_1_end == buckets.ends[0]);
    MY_ASSERT(ori_0_start == buckets.starts[1] && ori_1_start == buckets.starts[0]);
    MY_ASSERT(ori_0_len == buckets.lens[0] && ori_1_len == buckets.lens[1]);

    buckets.swap(1, 0);
    compare_bucket_to_ref(input1, buckets.at(0));
    compare_bucket_to_ref(input2, buckets.at(1));
    MY_ASSERT(ori_0_end == buckets.ends[0] && ori_1_end == buckets.ends[1]);
    MY_ASSERT(ori_0_start == buckets.starts[0] && ori_1_start == buckets.starts[1]);
    MY_ASSERT(ori_0_len == buckets.lens[0] && ori_1_len == buckets.lens[1]);
  }
}

void test_bucket_swap_one_empty() {
  TEST_HEADER();
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();
  uint8_t *ori_0_start = buckets.starts[0];
  uint8_t *ori_1_start = buckets.starts[1];
  uint8_t *ori_0_end = buckets.ends[0];
  uint8_t *ori_1_end = buckets.ends[1];

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(0));
  MY_ASSERT(ori_0_end == buckets.ends[1] && ori_1_end == buckets.ends[0]);
  MY_ASSERT(ori_0_start == buckets.starts[1] && ori_1_start == buckets.starts[0]);
  MY_ASSERT(buckets.begin(1) == buckets.end(1));

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(0));
  MY_ASSERT(ori_0_end == buckets.ends[0] && ori_1_end == buckets.ends[1]);
  MY_ASSERT(ori_0_start == buckets.starts[0] && ori_1_start == buckets.starts[1]);
  MY_ASSERT(buckets.begin(1) == buckets.end(1));
}

void test_decompression() {
  TEST_HEADER();
  DECLARE_INPUT;
  array<uint8_t, 1024> data = write_compressed_data();
  uint8_t* start=data.data();
  uint32_t bit_offset = 0;

  for(auto u : input) {
    auto int_len = decompress(start, bit_offset);
    start += (bit_offset + int_len.second) / 8;
    bit_offset = (bit_offset + int_len.second) % 8;
    LOG(u << " " << my_format(int_len) << " " << static_cast<void*>(start) << " " << bit_offset);
  }
}

void test_write_overflow() {
  TEST_HEADER();
  array<uint8_t, 1024> data = {{0}};
  uint8_t* start=data.data();
  uint32_t result1 = write_compressed(max_v_mask, start, 0, start+2);
  uint32_t result2 = write_compressed(max_v_mask, start, 2, start+4);
  MY_ASSERT(overflow_count(result1));
  MY_ASSERT(overflow_count(result2));
}

void test_generate_rand_number_input() {
  TEST_HEADER();
  auto input = generate_rand_uint_input(100);
  LOG(print_collection(input));
}

void test_uint32_to_bucket_int() {
  TEST_HEADER();
  auto input = generate_rand_uint_input(100);
  for(auto d : input) {
    auto result = uint32_to_bucket_int(d);
    LOG(my_format(d) << " " << my_format(result));
  }
}

void test_bucket_shift_bits() {
  TEST_HEADER();
  vector<uint8_t> input1 = {1,2,3,4,5,6,7};
  vector<uint8_t> input2 = {1,1,1,1,1,1,1};
  vector<uint8_t> input3 = {64,64,64};
  auto buckets = build_buckets();
  fill_bucket_raw(buckets, 0, input1);
  fill_bucket_raw(buckets, 1, input1);
  fill_bucket_raw(buckets, 2, input2);
  fill_bucket_raw(buckets, 3, input2);
  fill_bucket_raw(buckets, 4, input2);
  fill_bucket_raw(buckets, 5, input2);
  fill_bucket_raw(buckets, 6, input2);
  fill_bucket_raw(buckets, 7, input3);

  auto start_it = buckets.begin(0);
  start_it.shift_bits(24);
  buckets.shift_bits(0, start_it, 16);
  MY_ASSERT(std::equal(input1.begin() + 3, input1.end(), buckets.starts[0]+5));
  MY_ASSERT(std::equal(input1.begin(), input1.begin() + 3, buckets.starts[0]));
  MY_ASSERT(buckets.end(0).start == buckets.starts[0]+9);

  start_it = buckets.begin(1);
  start_it.shift_bits(24);
  buckets.shift_bits(1, start_it, -16);
  MY_ASSERT(std::equal(input1.begin() + 3, input1.end(), buckets.starts[1]+1));
  MY_ASSERT(buckets.starts[1][0] == 1);
  MY_ASSERT(buckets.end(1).start == buckets.starts[1]+5);

  vector<uint8_t> ref1 = {8,8,8,8,0};
  start_it = buckets.begin(2);
  start_it.shift_bits(24);
  buckets.shift_bits(2, start_it, 19);
  MY_ASSERT(std::equal(ref1.begin(), ref1.end(), buckets.starts[2]+5));
  MY_ASSERT(buckets.end(2).start == buckets.starts[2]+9);

  vector<uint8_t> ref2 = {33,32,32,32};
  start_it = buckets.begin(3);
  start_it.shift_bits(24);
  buckets.shift_bits(3, start_it, -11);
  MY_ASSERT(std::equal(ref2.begin(), ref2.end(), buckets.starts[3]+1));
  MY_ASSERT(buckets.end(3).start == buckets.starts[3]+5);

  start_it = buckets.begin(4);
  start_it.shift_bits(24);
  buckets.shift_bits(4, start_it, 3);
  MY_ASSERT(std::equal(ref1.begin(), ref1.end(), buckets.starts[4]+3));
  MY_ASSERT(buckets.end(4).start == buckets.starts[4]+7);

  start_it = buckets.begin(5);
  start_it.shift_bits(27);
  buckets.shift_bits(5, start_it, 3);
  MY_ASSERT(std::equal(ref1.begin(), ref1.end(), buckets.starts[5]+3));
  MY_ASSERT(buckets.end(5).start == buckets.starts[5]+7);

  vector<uint8_t> ref4 = {32,32,32,0};
  start_it = buckets.begin(6);
  start_it.shift_bits(29);
  buckets.shift_bits(6, start_it, -3);
  MY_ASSERT(std::equal(ref4.begin(), ref4.end(), buckets.starts[6]+3));
  MY_ASSERT(buckets.end(6).start == buckets.starts[6]+6);

  vector<uint8_t> ref3 = {0,2,2,2};
  start_it = buckets.begin(7);
  buckets.shift_bits(7, start_it, 3);
  MY_ASSERT(std::equal(ref3.begin(), ref3.end(), buckets.starts[7]));
  MY_ASSERT(buckets.end(7).start == buckets.starts[7]+3);
}

void test_bucket_fail_shift_bits() {
  TEST_HEADER();
  auto buckets = build_buckets();
  vector<uint8_t> input1(buckets.capacity(0) / 8, 222);
  vector<uint8_t> input2 = {1,1,1,1,1,1,1};
  fill_bucket_raw(buckets, 0, input1);
  fill_bucket_raw(buckets, 1, input2);

  auto start_it = buckets.begin(0);
  uint32_t ok = buckets.shift_bits(0, start_it, 1);
  MY_ASSERT(overflow_count(ok));
  MY_ASSERT(buckets.end(0).start == buckets.ends[0]);

  ok = buckets.shift_bits(0, start_it, 11);
  MY_ASSERT(overflow_count(ok));

  start_it.shift_bits(77);
  ok = buckets.shift_bits(0, start_it, -66);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(buckets.end(0).start == buckets.ends[0]-9);

  start_it = buckets.begin(1);
  ok = buckets.shift_bits(1, start_it, -1);
  MY_ASSERT(overflow_count(ok));

  start_it.shift_bits(24);
  ok = buckets.shift_bits(1, start_it, -25);
  MY_ASSERT(overflow_count(ok));
  MY_ASSERT(start_it.start == buckets.starts[1]+3);
}

void test_bucket_overflow_shift_bits() {
  TEST_HEADER();
  auto buckets = build_buckets();
  fill_bucket_to_max(buckets, 0, comp_max_1 - 1);
  uint8_t ov_shft = buckets.end(0).bit_offset ? (9 - buckets.end(0).bit_offset) : 1;

  auto end_it = buckets.end(0);
  uint32_t ok = buckets.shift_bits(0, end_it, ov_shft);
  MY_ASSERT(overflow_count(ok));
  MY_ASSERT(buckets.starts[1][0] == 0);

  auto start_it = buckets.begin(0);
  ok = buckets.shift_bits(0, start_it, ov_shft);
  MY_ASSERT(overflow_count(ok));
  MY_ASSERT(buckets.starts[1][0] == 0);

  end_it = buckets.end(0);
  start_it = buckets.end(0);
  start_it.shift_bits(-17);
  ok = buckets.shift_bits(0, start_it, 0);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(buckets.starts[1][0] == 0);
  MY_ASSERT(end_it == buckets.end(0));

  end_it = buckets.end(0);
  end_it.shift_bits(-8);
  buckets.update(0, end_it);
  end_it.shift_bits(-1);
  ok = buckets.shift_bits(0, end_it, 8 + ov_shft - 2);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(buckets.starts[1][0] == 0);
  MY_ASSERT(end_it != buckets.end(0));

  fill_bucket_to_max(buckets, 0, comp_max_1 - 1);
  end_it = buckets.end(0);
  end_it.shift_bits(-8);
  buckets.update(0, end_it);
  ok = buckets.shift_bits(0, end_it, 8 + ov_shft - 1);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(buckets.starts[1][0] == 0);

  fill_bucket_to_max(buckets, 0, comp_max_1 - 1);
  end_it = buckets.end(0);
  end_it.shift_bits(-8);
  buckets.update(0, end_it);
  end_it.shift_bits(-1);
  ok = buckets.shift_bits(0, end_it, 8 + ov_shft - 1);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(buckets.starts[1][0] == 0);

  fill_bucket_to_max(buckets, 0, comp_max_1 - 1);
  end_it = buckets.end(0);
  end_it.shift_bits(-8);
  buckets.update(0, end_it);
  end_it.shift_bits(-16);
  ok = buckets.shift_bits(0, end_it, 8 + ov_shft - 1);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(buckets.starts[1][0] == 0);

  clean_bucket_raw(buckets, 0);
  fill_bucket_to_max(buckets, 1, comp_max_1 - 1);
  start_it = buckets.begin(1);
  start_it.shift_bits(17);
  ok = buckets.shift_bits(1, start_it, -8);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(*(buckets.starts[1]-1) == 0);

  clean_bucket_raw(buckets, 0);
  fill_bucket_to_max(buckets, 1, comp_max_1 - 1);
  start_it = buckets.begin(1);
  start_it.shift_bits(16);
  ok = buckets.shift_bits(1, start_it, -8);
  MY_ASSERT(!overflow_count(ok));
  MY_ASSERT(*(buckets.starts[1]-1) == 0);
}

void test_add_number() {
  TEST_HEADER();
  vector<uint32_t> input = { 0, 0, 1, 1, 3, 5, 5, 7, 9, 12, 14, 14 };
  auto buckets = build_buckets();

  for(auto n : input) {
    auto result = buckets.add_number(n);
    MY_ASSERT(!overflow_count(result));
  }
  compare_deltas_to_ref(input, buckets.at(0));

  buckets.clear();
  for(uint32_t i=0; i < 8*(buckets.ends[0] - buckets.starts[0]) / compress_len(0); ++i) {
    auto result = buckets.add_number(0);
    MY_ASSERT(!overflow_count(result));
  }
  auto result = buckets.add_number(0);
  MY_ASSERT(overflow_count(result));
}

void test_add_number_fuzzy() {
  TEST_HEADER();
  auto buckets = build_buckets();
  for(uint32_t attempt = 0;
      attempt < 2000;
      ++attempt, buckets.clear()) {
    auto input = generate_rand_uint_input(0, bucket_max_value);

    for(auto value : input) {
      auto result = buckets.add_number(value);
      MY_ASSERT(!overflow_count(result));
    }

    sort(input.begin(), input.end());
    compare_deltas_to_ref(input, buckets.at(0));
  }
}

void test_bucket_both_extend_fuzzy() {
  TEST_HEADER();
  auto buckets = build_buckets();

  auto test_one_side = [&buckets](uint32_t grow_idx, uint32_t shrink_idx, function<uint32_t(uint32_t, uint32_t)> extend) {
    buckets.clear();
    for(uint32_t attempt = 0; attempt < 10000; ++attempt, buckets.clear()) {
      auto input1 = generate_rand_uint_input(0, bucket_max_value);
      auto input2 = generate_rand_uint_input(0, bucket_max_value);
      if(input1.empty() || input2.empty()) continue;

      fill_bucket(buckets, shrink_idx, input1);
      fill_bucket(buckets, grow_idx, input2);
      auto ori_shrink_cap = buckets.capacity(shrink_idx);
      auto ori_grow_cap = buckets.capacity(grow_idx);
      uint32_t amount = 7717 % (buckets.available(shrink_idx)/8) + 1;

      extend(grow_idx, amount);
      //LOG("amount=" << amount << " new_shrink_cap=" << buckets.capacity(shrink_idx)/8 << " new_grow_cap=" << buckets.capacity(grow_idx)/8);
      compare_bucket_to_ref(input1, buckets.at(shrink_idx));
      compare_bucket_to_ref(input2, buckets.at(grow_idx));
      MY_ASSERT(ori_shrink_cap - 8*amount == buckets.capacity(shrink_idx));
      MY_ASSERT(ori_grow_cap   + 8*amount == buckets.capacity(grow_idx));
    }
  };
  test_one_side(1, 0, [&](uint32_t i, uint32_t a) { return buckets.r_extend(i, a); });
  test_one_side(0, 1, [&](uint32_t i, uint32_t a) { return buckets.extend(i, a); });
}

void test_bucket_both_extend_fail() {
  TEST_HEADER();
  auto buckets = build_buckets();
  uint32_t ok = buckets.r_extend(0, 1);
  MY_ASSERT(overflow_count(ok) == 1);
  ok = buckets.extend(bucket_len - 1, 1);
  MY_ASSERT(overflow_count(ok) == 1);

  vector<uint32_t> input(buckets.capacity(0) / compress_len(1), 1);
  fill_bucket(buckets, 0, input);
  ok = buckets.r_extend(1, 5);
  MY_ASSERT(overflow_count(ok));

  buckets.clear();
  vector<uint32_t> input2(buckets.capacity(1) / compress_len(1), 1);
  fill_bucket(buckets, 1, input);
  ok = buckets.extend(0, 5);
  MY_ASSERT(overflow_count(ok));
}

void test_bucket_add_and_rebalance() {
  TEST_HEADER();
  auto buckets = build_buckets();
  uint32_t number = 1;
  uint32_t max_cap = buckets.select_bigger([&](uint32_t i) { return buckets.capacity(i); }).second;
  uint32_t to_add_count = 3 * max_cap / compress_len(number) + 1;
  
  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, number);
    MY_ASSERT(!overflow_count(ok));
  }
  //auto big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  //LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));

  buckets.clear();
  number += bucket_val_mask * (bucket_len - 1);

  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, number);
    MY_ASSERT(!overflow_count(ok));
  }
  //big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  //LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));

  buckets.clear();
  number = max_v_mask - 1;
  to_add_count = 2 * max_cap / compress_len(1) - 1;

  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, number);
    MY_ASSERT(!overflow_count(ok));
  }
  //big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  //LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));
}

void test_bucket_add_and_rebalance_fuzzy() {
  TEST_HEADER();
  const uint32_t fill_val = 0;
  const double ratio = 1.0 * comp_len_1 / comp_len_3;
  const double fill_frac = 0.75;
  const double balance_frac = ((1-fill_frac) * ratio) * 1.67; // tweaked to almost overflow buckets

  auto buckets = build_buckets();
  auto ref_buckets = build_buckets();
  uint32_t ref_cap = ref_buckets.capacity(0) / compress_len(fill_val);
  vector<uint32_t> input(fill_frac * ref_cap, fill_val);
  for(uint32_t i=0; i<bucket_len; ++i) 
    fill_bucket(ref_buckets, i, input);
  auto ref_stats = ref_buckets.calculate_stats();
  
  for(uint32_t attempt = 0; attempt < 20; ++attempt) {
    buckets.clear();
    buckets.buffer = ref_buckets.buffer;
    buckets.lens = ref_buckets.lens;
    auto extra_input = generate_rand_uint_input(balance_frac * ref_cap * bucket_len);
    auto remove_from = std::remove_if(extra_input.begin(), extra_input.end(),
       [](uint32_t d) { return !(d % bucket_val_mask); });
    extra_input.erase(remove_from, extra_input.end());

    for(auto number : extra_input) {
      uint32_t ok = add_rebalance_if_needed(buckets, number);
      MY_ASSERT(!overflow_count(ok));
    }

    auto stats = buckets.calculate_stats();
    LOG("attempt " << attempt << " : " << print_stats(stats));
    MY_ASSERT(stats.tot_len_kb > ref_stats.tot_len_kb);
    MY_ASSERT(stats.tot_avail_kb < ref_stats.tot_avail_kb);

    vector<uint32_t> result(buffer_len * 8 / comp_len_1);
    std::copy(buckets.global_begin(), buckets.global_end(), result.begin());
    MY_ASSERT(std::is_sorted(buckets.global_begin(), buckets.global_end()));

    //LOG(stats.item_count << " / " << extra_input.size());
    std::sort(extra_input.begin(), extra_input.end());
    uint32_t hit_count = 0;
    auto ref_it = extra_input.begin();
    auto start_it = extra_input.begin();

    for(auto dec : result) {
      ref_it = lower_bound(ref_it, extra_input.end(), dec);
      MY_ASSERT(ref_it != extra_input.end());
      //if (hit_count % 1000 == 0)
      //  LOG(my_format(dec) << " / " << my_format(*ref_it));
      if(dec != *ref_it && !(*ref_it % bucket_val_mask)) {
        LOG(" fail at " << std::distance(start_it, ref_it) << " : " << my_format(dec) << " != " << my_format(*ref_it));
        MY_ASSERT(dec == *ref_it);
      }
      hit_count += (dec == *ref_it);
    }
    LOG("hit_count=" << hit_count << " / extra_input_size=" << extra_input.size());
    MY_ASSERT(hit_count == extra_input.size());
  }
}

void sort_one_million_in_one_mb() {
  uint32_t error_count = 0;
  for(uint32_t attempt = 0; attempt < 1; ++attempt) {
    auto input = generate_rand_uint_input(input_len);
    const auto buckets = order_numbers_into_buckets(input);
    LOG("buckets stats = " << print_stats(buckets.calculate_stats()));
    
    auto global_it = buckets.global_begin();
    std::sort(input.begin(), input.end());

    for(uint32_t idx=0; idx < input.size(); ++idx, ++global_it) {
      MY_ASSERT(global_it != buckets.global_end());
      //LOG("at " << idx << " : " << my_format(input[idx]) << " != " << my_format(*global_it));
      if(input[idx] != *global_it) 
        ++error_count;
    }
  }
  LOG("error_count  = " << error_count);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void silly_test_all() {
  test_compression();
  test_generate_rand_number_input();
  test_uint32_to_bucket_int();
  test_decompression();
  test_write_overflow();
}

/* self validating tests */
void test_validation_all() {
  test_compress_len();
  test_compression_exhaustive();
  test_codec_fuzzy();
  test_bucket_iteration();
  test_bucket_algorithm();
  test_add_number();
  test_add_number_fuzzy();
  test_bucket_swap_one_empty();
  test_bucket_swap_same_len();
  test_bucket_swap_fuzzy();
  test_bucket_shift_bits();
  test_bucket_fail_shift_bits();
  test_bucket_overflow_shift_bits();
  test_bucket_both_extend_fail();
  test_bucket_both_extend_fuzzy();
  test_bucket_add_and_rebalance();
  test_bucket_add_and_rebalance_fuzzy();
}

int main(void) {
  std::cout << IOMANIPS;

  //test_validation_all();
  sort_one_million_in_one_mb();
  return 0;
}


