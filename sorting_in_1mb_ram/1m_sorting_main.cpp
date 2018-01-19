#include "1m_sorting.hpp"
#include "1m_sorting_print.hpp"
#include <algorithm>
#include <functional>
#include <random>
#include <cassert>

#define DECLARE_INPUT \
  vector<uint32_t> input = { 0, 1, 32, 64, 90, 100, 127, 128, 255, 256, 1024, 8291, 8292, 5000, 2097251, 2097252, 2097352, 99999999, rand_msk }

using namespace std;

void compare_bucket_to_ref(const vector<uint32_t>& ref_vect, ItContainer<BucketIt> bucket) {
  uint32_t idx = 0;
  for(auto value : bucket) {
    auto ref = ref_vect[idx++];
    if (ref != value) {
      LOG(ref << " " << value);
      assert(ref == value);
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
      assert(ref == sum);
    }
  }
}

void fill_bucket(Buckets& buckets, uint32_t target, const vector<uint32_t>& ref_vect) {
  auto write_it = buckets.at(target).begin();
  auto idx = 0;
  for(auto u : ref_vect) {
    uint32_t result = write_it.write_and_advance(u);
    idx ++;
    assert(!overflow_count(result));
  }
  buckets.update(target, write_it);
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

array<uint8_t, 1024> test_write_compression() {
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

void test_compression() {
  DECLARE_INPUT;
  for(auto u : input) {
    auto comp = compress(u);
    auto decomp = decompress(comp);
    LOG(print_collection(comp) << " " << u << " " << decomp);
  }
}

void test_compression_exhaustive() {
  for(uint32_t u=0; u<rand_msk; ++u) {
    auto comp = compress(u);
    auto decomp = decompress(comp);
    if (u != decomp) {
      LOG(print_collection(comp) << " " << u << " " << decomp);
      assert(u == decomp);
    }
  }
}

void test_bucket_iteration() {
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();
  compare_bucket_to_ref(input, buckets.at(0));
}

void test_bucket_algorithm() {
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();
  vector<uint32_t> to_find = {100, 0, 1, rand_msk};
  vector<uint32_t> to_miss = {333, 666, 289347};

  for(uint32_t u : to_find) {
    auto it = find(buckets.begin(0), buckets.end(0), u);
    if(it == buckets.end(0)) {
      LOG(u << " " << distance(it.start, buckets.starts[0]));
      assert(it != buckets.end(0));
    }
  }
  for(uint32_t u : to_miss) {
    auto it = find(buckets.begin(0), buckets.end(0), u);
    if(it != buckets.end(0)) {
      LOG(u << " " << distance(it.start, buckets.starts[0]));
      assert(it == buckets.end(0));
    }
  }

  auto sum_it = accumulate(buckets.begin(0), buckets.end(0), 0);
  auto sum_ref = accumulate(input.begin(), input.end(), 0);
  assert(sum_it == sum_ref);
}

void test_codec_fuzzy() {
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
      assert(decimals[idx].first == value);
    }
    idx += 1;
  }
}

void test_bucket_swap_same_len() {
  auto buckets = build_and_fill_first_bucket();

  DECLARE_INPUT;
  auto shuf_input = input;
  random_shuffle(shuf_input.begin(), shuf_input.end());
  fill_bucket(buckets, 3, shuf_input);

  auto ori_0_end = buckets.end(0);
  auto ori_3_end = buckets.end(3);

  buckets.swap(0, 3);
  assert(ori_0_end == buckets.end(3) && ori_3_end == buckets.end(0));
  compare_bucket_to_ref(shuf_input, buckets.at(3));
  compare_bucket_to_ref(input, buckets.at(0));

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
    uint32_t ori_0_len = buckets.lens[0];
    uint32_t ori_1_len = buckets.lens[1];
    uint8_t *ori_0_start = buckets.starts[0];
    uint8_t *ori_1_start = buckets.starts[1];
    uint8_t *ori_0_end = buckets.ends[0];
    uint8_t *ori_1_end = buckets.ends[1];

    buckets.swap(0, 1);
    compare_bucket_to_ref(input1, buckets.at(0));
    compare_bucket_to_ref(input2, buckets.at(1));
    assert(ori_0_end == buckets.ends[1] && ori_1_end == buckets.ends[0]);
    assert(ori_0_start == buckets.starts[1] && ori_1_start == buckets.starts[0]);
    assert(ori_0_len == buckets.lens[0] && ori_1_len == buckets.lens[1]);

    buckets.swap(1, 0);
    compare_bucket_to_ref(input1, buckets.at(0));
    compare_bucket_to_ref(input2, buckets.at(1));
    assert(ori_0_end == buckets.ends[0] && ori_1_end == buckets.ends[1]);
    assert(ori_0_start == buckets.starts[0] && ori_1_start == buckets.starts[1]);
    assert(ori_0_len == buckets.lens[0] && ori_1_len == buckets.lens[1]);
  }
}

void test_bucket_swap_one_empty() {
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();
  uint8_t *ori_0_start = buckets.starts[0];
  uint8_t *ori_1_start = buckets.starts[1];
  uint8_t *ori_0_end = buckets.ends[0];
  uint8_t *ori_1_end = buckets.ends[1];

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(0));
  assert(ori_0_end == buckets.ends[1] && ori_1_end == buckets.ends[0]);
  assert(ori_0_start == buckets.starts[1] && ori_1_start == buckets.starts[0]);
  assert(buckets.begin(1) == buckets.end(1));

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(0));
  assert(ori_0_end == buckets.ends[0] && ori_1_end == buckets.ends[1]);
  assert(ori_0_start == buckets.starts[0] && ori_1_start == buckets.starts[1]);
  assert(buckets.begin(1) == buckets.end(1));
}

void test_decompression() {
  DECLARE_INPUT;
  array<uint8_t, 1024> data = test_write_compression();
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
  array<uint8_t, 1024> data = {0};
  uint8_t* start=data.data();
  uint32_t result1 = write_compressed(rand_msk, start, 0, start+2);
  uint32_t result2 = write_compressed(rand_msk, start, 2, start+4);
  assert(overflow_count(result1));
  assert(overflow_count(result2));
}

void test_generate_rand_decimal_input() {
  auto input = generate_rand_decimal_input(100);
  LOG(print_collection(input));
}

void test_decimal_to_bucket_int() {
  auto input = generate_rand_decimal_input(100);
  for(auto d : input) {
    auto result = decimal_to_bucket_int(d);
    LOG(my_format(d) << " " << my_format(result));
  }
}

void test_add_number() {
  vector<uint32_t> input = { 0, 0, 1, 1, 3, 5, 5, 7, 9, 12, 14, 14 };
  auto buckets = build_buckets();

  for(auto n : input) {
    decimal_t decimal = {n, 0};
    auto result = buckets.add_number(decimal);
    assert(!overflow_count(result));
  }
  compare_deltas_to_ref(input, buckets.at(0));

  buckets.clear();
  decimal_t decimal = {0, 0};
  for(uint32_t i=0; i < 8*(buckets.ends[0] - buckets.starts[0]) / compress_len(0); ++i) {
    auto result = buckets.add_number(decimal);
    assert(!overflow_count(result));
  }
  auto result = buckets.add_number(decimal);
  assert(overflow_count(result));
}

void test_add_number_fuzzy() {
  auto buckets = build_buckets();
  for(uint32_t attempt = 0;
      attempt < 10000;
      ++attempt, buckets.clear()) {
    auto input = generate_rand_uint_input(0, bucket_max_value);

    for(auto value : input) {
      auto result = buckets.add_number(decimal_t{value, 0});
      assert(!overflow_count(result));
    }

    sort(input.begin(), input.end());
    compare_deltas_to_ref(input, buckets.at(0));
  }
}

void test_bucket_both_extend_fuzzy() {
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
      assert(ori_shrink_cap - amount == buckets.ends[shrink_idx] - buckets.starts[shrink_idx]); 
      assert(ori_grow_cap + amount == buckets.ends[grow_idx] - buckets.starts[grow_idx]); 
    }
  };
  test_one_side(1, 0, [&](uint32_t i, uint32_t a) { return buckets.r_extend(i, a); });
  test_one_side(0, 1, [&](uint32_t i, uint32_t a) { return buckets.extend(i, a); });
}

void test_bucket_both_extend_fail() {
  auto buckets = build_buckets();
  uint32_t ok = buckets.r_extend(0, 1);
  assert(overflow_count(ok) == 1);
  ok = buckets.extend(Buckets::bucket_len - 1, 1);
  assert(overflow_count(ok) == 1);

  vector<uint32_t> input(buckets.capacity(0) / compress_len(1), 1);
  fill_bucket(buckets, 0, input);
  ok = buckets.r_extend(1, 5);
  assert(overflow_count(ok));

  buckets.clear();
  vector<uint32_t> input2(buckets.capacity(1) / compress_len(1), 1);
  fill_bucket(buckets, 1, input);
  ok = buckets.extend(0, 5);
  assert(overflow_count(ok));
}

void test_bucket_add_and_rebalance() {
  auto buckets = build_buckets();
  decimal_t decimal = {1, 0};
  uint32_t max_cap = buckets.select_bigger([&](uint32_t i) { return buckets.capacity(i); }).second;
  uint32_t to_add_count = 3 * max_cap / compress_len(1) + 1;
  
  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, decimal);
    assert(!overflow_count(ok));
  }
  auto big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));

  buckets.clear();
  decimal.second = decimal_places - 1;

  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, decimal);
    assert(!overflow_count(ok));
  }
  big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));

  buckets.clear();
  decimal = make_pair(rand_msk - 1, decimal_places - 1);
  to_add_count = 2 * max_cap / compress_len(1) - 1;

  for(uint32_t i=0; i<to_add_count; ++i) {
    uint32_t ok = add_rebalance_if_needed(buckets, decimal);
    assert(!overflow_count(ok));
  }
  big_bucket = buckets.select_first([&](uint32_t i) { return buckets.capacity(i); });
  LOG("### to_add_count=" << to_add_count << " max_cap=" << max_cap << " big_bucket=" << my_format(big_bucket));
}

void test_bucket_add_and_rebalance_fuzzy() {
  const uint32_t fill_val = 0;
  auto buckets = build_buckets();
  auto ref_buckets = build_buckets();
  uint32_t ref_cap = ref_buckets.capacity(0) / compress_len(fill_val);
  vector<uint32_t> input(0.9 * ref_cap, fill_val);
  for(uint32_t i=0; i<Buckets::bucket_len; ++i) 
    fill_bucket(ref_buckets, i, input);
  auto ref_stats = ref_buckets.calculate_stats();
  
  for(uint32_t attempt = 0; attempt < 10; ++attempt) {
    buckets.clear();
    buckets.buffer = ref_buckets.buffer;
    buckets.lens = ref_buckets.lens;
    auto extra_input = generate_rand_decimal_input(0.05 * Buckets::bucket_len * ref_cap);
    extra_input.erase(std::remove_if(extra_input.begin(), extra_input.end(), 
      [](decimal_t d) { return d.first % bucket_val_mask == 0; }));

    for(auto decimal : extra_input) {
      uint32_t ok = add_rebalance_if_needed(buckets, decimal);
      assert(!overflow_count(ok));
    }

    std::sort(extra_input.begin(), extra_input.end(), comp_decimal);
    //LOG(print_collection(extra_input));
    auto stats = buckets.calculate_stats();
    assert(stats.tot_len > ref_stats.tot_len);
    assert(stats.tot_avail < ref_stats.tot_avail);

    vector<decimal_t> result(input_len);
    std::copy(buckets.global_begin(), buckets.global_end(), result.begin());
    assert(std::is_sorted(buckets.global_begin(), buckets.global_end(), comp_decimal));

    LOG(result.size() << " / " << extra_input.size());
    uint32_t hit_count = 0;
    auto ref_it = extra_input.begin();
    for(auto dec : result) {
      ref_it = lower_bound(ref_it, extra_input.end(), dec, comp_decimal);
      assert(ref_it != extra_input.end());
      if(dec == *ref_it) {
        if (hit_count % 1000 == 0)
          LOG(my_format(dec) << " / " << my_format(*ref_it));
        ++hit_count;
      }
    }
    LOG(hit_count << " / " << extra_input.size());
    assert(hit_count == extra_input.size());
  }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(void) {
  std::cout << IOMANIPS;
  //test_compression();
  //test_generate_rand_decimal_input();
  //test_decimal_to_bucket_int();
  //test_write_compression();
  //test_decompression();
  //test_write_overflow();

  /* self validating tests */

  //test_bucket_iteration();
  //test_bucket_algorithm();
  //test_compression_exhaustive();
  //test_bucket_swap_one_empty();
  //test_bucket_swap_same_len();
  //test_bucket_swap_fuzzy();
  //test_codec_fuzzy();
  //test_add_number();
  //test_add_number_fuzzy();
  //test_bucket_both_extend_fail();
  //test_bucket_both_extend_fuzzy();
  //test_bucket_add_and_rebalance();
  test_bucket_add_and_rebalance_fuzzy();
  return 0;
}


