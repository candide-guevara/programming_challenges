#include "1m_sorting.hpp"
#include "1m_sorting_print.hpp"
#include <algorithm>
#include <functional>
#include <random>

#define DECLARE_INPUT \
  vector<uint32_t> input = { 0, 1, 32, 64, 90, 100, 127, 128, 255, 256, 1024, 8291, 8292, 5000, 2097251, 2097252, 2097352, 99999999, rand_msk }

using namespace std;

void compare_bucket_to_ref(const vector<uint32_t>& ref_vect, ItContainer<BucketIt> bucket) {
  uint32_t idx = 0;
  for(auto value : bucket) {
    auto ref = ref_vect[idx++];
    if (ref != value) {
      LOG(ref << " " << value);
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
      LOG(n << " " << sum << " " << ref);
      MY_ASSERT(ref == sum);
    }
  }
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

vector<decimal_t> generate_rand_decimal_input(uint32_t len, uint32_t max_value=max_rand) {
  vector<decimal_t> result;
  result.resize(len);
  random_device rd;
  mt19937 gen(rd());
  uniform_int_distribution<> dist(0, max_value);

  for(auto& i : result) {
    auto source = dist(gen);
    i.first = source % rand_msk;
    i.second = source / rand_msk;
  }
  return result;
}

vector<uint32_t> generate_rand_uint_input(uint32_t len, uint32_t max_item_val) {
  random_device rd;
  mt19937 gen(rd());
  uniform_int_distribution<> dist(0, max_item_val);

  if(len == 0)
    len = dist(gen) % 240;
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
  for(uint32_t u=0; u<rand_msk; ++u) {
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
  int32_t lower_bound1 = (comp_max_1>>1) <= bias ? (comp_max_1>>1) : bias;
  int32_t lower_bound2 = (comp_max_2>>1) <= bias ? (comp_max_2>>1) : bias;

  for(int32_t n=-lower_bound1; n<(int32_t)(comp_max_1>>1); ++n)
    MY_ASSERT(compress_len(bias + n) == comp_len_1);
  for(int32_t n=-lower_bound2; n<(int32_t)(comp_max_2>>1); ++n)
    if(n<-(int32_t)(comp_max_1>>1) || n>=(int32_t)(comp_max_1>>1))
      MY_ASSERT(compress_len(bias + n) == comp_len_2);
  for(auto n : generate_rand_uint_input(1000000, comp_max_3/2 - 1))
    if(n>=(int32_t)(comp_max_2>>1))
      MY_ASSERT(compress_len(bias + n) == comp_len_3);

  MY_ASSERT(compress_len(bias + comp_max_3/2) == comp_len_4);
  MY_ASSERT(compress_len(bias + comp_max_3/2 + 1) == comp_len_4);
  MY_ASSERT(compress_len(bias + comp_max_3/2 - 1) == comp_len_3);
  MY_ASSERT(compress_len(bias + comp_max_3) == comp_len_4);
}

void test_bucket_algorithm() {
  TEST_HEADER();
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();
  vector<uint32_t> to_find = {100, 0, 1, rand_msk};
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
  auto decimals = generate_rand_decimal_input(1000 * 1000 * 10);
  vector<uint8_t> buffer;
  buffer.resize(decimals.size() * 4);
  BucketIt write_it {buffer.data(), buffer.data() + buffer.size(), 0};
  BucketIt start_it {buffer.data(), buffer.data() + buffer.size(), 0};

  for(auto u : decimals)
    write_it.write_and_advance(u.first);

  ItContainer<BucketIt> container {start_it, write_it};
  for(auto value : container) { 
    if(decimals[idx].first != value) {
      LOG(decimals[idx].first << " != " << value);
      MY_ASSERT(decimals[idx].first == value);
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
  uint32_t result1 = write_compressed(rand_msk, start, 0, start+2);
  uint32_t result2 = write_compressed(rand_msk, start, 2, start+4);
  MY_ASSERT(overflow_count(result1));
  MY_ASSERT(overflow_count(result2));
}

void test_generate_rand_decimal_input() {
  TEST_HEADER();
  auto input = generate_rand_decimal_input(100);
  LOG(print_collection(input));
}

void test_decimal_to_bucket_int() {
  TEST_HEADER();
  auto input = generate_rand_decimal_input(100);
  for(auto d : input) {
    auto result = decimal_to_bucket_int(d);
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

void test_add_number() {
  TEST_HEADER();
  vector<uint32_t> input = { 0, 0, 1, 1, 3, 5, 5, 7, 9, 12, 14, 14 };
  auto buckets = build_buckets();

  for(auto n : input) {
    decimal_t decimal = {n, 0};
    auto result = buckets.add_number(decimal);
    MY_ASSERT(!overflow_count(result));
  }
  compare_deltas_to_ref(input, buckets.at(0));

  buckets.clear();
  decimal_t decimal = {0, 0};
  for(uint32_t i=0; i < 8*(buckets.ends[0] - buckets.starts[0]) / compress_len(0); ++i) {
    auto result = buckets.add_number(decimal);
    MY_ASSERT(!overflow_count(result));
  }
  auto result = buckets.add_number(decimal);
  MY_ASSERT(overflow_count(result));
}

void test_add_number_fuzzy() {
  TEST_HEADER();
  auto buckets = build_buckets();
  for(uint32_t attempt = 0;
      attempt < 10000;
      ++attempt, buckets.clear()) {
    auto input = generate_rand_uint_input(0, bucket_max_value);

    for(auto value : input) {
      auto result = buckets.add_number(decimal_t{value, 0});
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
      fill_bucket(buckets, shrink_idx, input1);
      fill_bucket(buckets, grow_idx, input2);
      auto ori_shrink_cap = buckets.ends[shrink_idx] - buckets.starts[shrink_idx];
      auto ori_grow_cap = buckets.ends[grow_idx] - buckets.starts[grow_idx];
      uint32_t amount = attempt % 10 + 1;

      extend(grow_idx, amount);
      compare_bucket_to_ref(input1, buckets.at(shrink_idx));
      compare_bucket_to_ref(input2, buckets.at(grow_idx));
      MY_ASSERT(ori_shrink_cap - amount == buckets.ends[shrink_idx] - buckets.starts[shrink_idx]); 
      MY_ASSERT(ori_grow_cap + amount == buckets.ends[grow_idx] - buckets.starts[grow_idx]); 
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
  decimal_t decimal = {1, 0};
  uint32_t max_cap = buckets.select_bigger([&](uint32_t i) { return buckets.capacity(i); }).second;
  uint32_t to_add_count = 3 * max_cap / compress_len(1) + 1;
  
  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, decimal);
    MY_ASSERT(!overflow_count(ok));
  }
  //auto big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  //LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));

  buckets.clear();
  decimal.second = decimal_places - 1;

  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, decimal);
    MY_ASSERT(!overflow_count(ok));
  }
  //big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  //LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));

  buckets.clear();
  decimal = make_pair(rand_msk - 1, decimal_places - 1);
  to_add_count = 2 * max_cap / compress_len(1) - 1;

  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, decimal);
    MY_ASSERT(!overflow_count(ok));
  }
  //big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  //LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));
}

void test_bucket_add_and_rebalance_fuzzy() {
  TEST_HEADER();
  const uint32_t fill_val = 0;
  auto buckets = build_buckets();
  auto ref_buckets = build_buckets();
  uint32_t ref_cap = ref_buckets.capacity(0) / compress_len(fill_val);
  vector<uint32_t> input(0.9 * ref_cap, fill_val);
  for(uint32_t i=0; i<bucket_len; ++i) 
    fill_bucket(ref_buckets, i, input);
  auto ref_stats = ref_buckets.calculate_stats();
  
  for(uint32_t attempt = 0; attempt < 20; ++attempt) {
    buckets.clear();
    buckets.buffer = ref_buckets.buffer;
    buckets.lens = ref_buckets.lens;
    auto extra_input = generate_rand_decimal_input(0.05 * bucket_len * ref_cap);
    extra_input.erase(std::remove_if(extra_input.begin(), extra_input.end(), 
      [](decimal_t d) { return d.first % bucket_val_mask == 0; }));

    for(auto decimal : extra_input) {
      uint32_t ok = add_rebalance_if_needed(buckets, decimal);
      MY_ASSERT(!overflow_count(ok));
    }

    std::sort(extra_input.begin(), extra_input.end(), comp_decimal);
    //LOG(print_collection(extra_input));
    auto stats = buckets.calculate_stats();
    MY_ASSERT(stats.tot_len > ref_stats.tot_len);
    MY_ASSERT(stats.tot_avail < ref_stats.tot_avail);

    vector<decimal_t> result(input_len);
    std::copy(buckets.global_begin(), buckets.global_end(), result.begin());
    MY_ASSERT(std::is_sorted(buckets.global_begin(), buckets.global_end(), comp_decimal));

    //LOG(result.size() << " / " << extra_input.size());
    uint32_t hit_count = 0;
    auto ref_it = extra_input.begin();
    for(auto dec : result) {
      ref_it = lower_bound(ref_it, extra_input.end(), dec, comp_decimal);
      MY_ASSERT(ref_it != extra_input.end());
      if(dec == *ref_it) {
        //if (hit_count % 1000 == 0)
        //  LOG(my_format(dec) << " / " << my_format(*ref_it));
        ++hit_count;
      }
    }
    //LOG(hit_count << " / " << extra_input.size());
    MY_ASSERT(hit_count == extra_input.size());
  }
}

void sort_one_million_in_one_mb() {
  uint32_t error_count = 0;
  for(uint32_t attempt = 0; attempt < 1; ++attempt) {
    auto input = generate_rand_decimal_input(input_len);
    const auto buckets = order_numbers_into_buckets(input);
    LOG("buckets stats = " << print_stats(buckets.calculate_stats()));
    
    auto global_it = buckets.global_begin();
    std::sort(input.begin(), input.end(), comp_decimal);

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
  test_generate_rand_decimal_input();
  test_decimal_to_bucket_int();
  test_decompression();
  test_write_overflow();
}

/* self validating tests */
void test_validation_all() {
  test_compress_len();
  test_bucket_iteration();
  test_bucket_algorithm();
  test_compression_exhaustive();
  test_bucket_swap_one_empty();
  test_bucket_swap_same_len();
  test_bucket_swap_fuzzy();
  test_codec_fuzzy();
  test_bucket_shift_bits();
  test_bucket_fail_shift_bits();
  test_add_number();
  test_add_number_fuzzy();
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


