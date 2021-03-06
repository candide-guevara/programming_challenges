#pragma once
#include <array>
#include <iterator>
#include <map>
#include <type_traits>
#include <vector>
#include <utility>
#include <cassert>

#ifdef NDEBUG
#define MY_ASSERT(x) do { (void)sizeof(x); } while (0)
#else
#define MY_ASSERT(x) assert(x)
#endif

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

const static size_t input_len  = 1000000;
const static size_t max_v_mask  = 100000000;
const static size_t comp_len_1 = 8;
const static size_t comp_len_2 = 16;
const static size_t comp_len_3 = 40;
const static size_t comp_len_4 = 40;
const static size_t comp_max_1 = 252;
const static size_t comp_max_2 = 1019;
const static size_t comp_max_3 = (1 << 29);
const static size_t comp_max_4 = (1 << 30);
const static size_t bucket_len = 1000;
const static size_t safe_bucket_inc = 2 * comp_len_3 / 8;
const static size_t bucket_val_mask = max_v_mask / bucket_len;
const static size_t bucket_max_value = bucket_val_mask - 1;
const static size_t buffer_len = (1024 + 32) * 1024;//input_len * 1.90;
const static size_t bias = max_v_mask / input_len;

using comp_int_t = std::array<uint8_t, comp_len_4/8>;
using decimal_t = uint32_t;
using int_len_t = std::pair<uint32_t, uint32_t>;
using bucket_int_t = std::pair<uint32_t, uint32_t>;
using bucket_stat_t = std::pair<uint32_t, double>;

struct Buckets;

struct BucketIt {
  uint8_t* start, *end;
  uint8_t bit_offset;
  uint32_t value;

  using value_type = uint32_t;
  using difference_type = int32_t;
  using pointer = const uint32_t*;
  using reference = const uint32_t&;
  using iterator_category = std::forward_iterator_tag;

  void shift_bits(int32_t len);
  uint32_t write_and_advance(uint32_t number);
  int_len_t get_and_advance();
  reference operator*();
  BucketIt& operator++();
  BucketIt operator++(int);

  bool operator==(const BucketIt& rhs) const;
  bool operator!=(const BucketIt& rhs) const;
  int32_t operator-(const BucketIt& rhs) const;
};

struct GlobalIt {
  const Buckets* buckets;
  uint32_t cur_bucket;
  BucketIt internal_it;
  decimal_t value;

  using value_type = decimal_t;
  using difference_type = int32_t;
  using pointer = const decimal_t*;
  using reference = const decimal_t&;
  using iterator_category = std::forward_iterator_tag;

  reference operator*();
  GlobalIt& operator++();
  GlobalIt operator++(int);

  bool operator==(const GlobalIt& rhs) const;
  bool operator!=(const GlobalIt& rhs) const;
};

template<class I>
struct ItContainer {
  I start_it;
  I end_it;
  I begin() const { return start_it; }
  I end() const { return end_it; }
};

struct StatBuckets {
  using Histo = std::map<uint32_t, uint32_t>;
  uint32_t item_count;
  bucket_stat_t min_cap_kb, min_avail_byte;
  bucket_stat_t max_cap_kb, max_avail_byte;
  double avg_cap_byte, avg_avail_byte;
  double std_cap_byte, std_avail_byte;
  double tot_len_kb, tot_avail_kb;
  Histo len_histo, val_histo;
};

struct Buckets {
  std::vector<uint8_t> buffer;
  std::vector<uint8_t*> starts, ends;
  std::vector<uint32_t> lens;

  ItContainer<BucketIt> at(uint32_t idx) const;
  ItContainer<GlobalIt> global_at() const;
  BucketIt begin(uint32_t) const;
  BucketIt end(uint32_t) const;
  GlobalIt global_begin() const;
  GlobalIt global_end() const;

  void clear();
  void update(uint32_t idx, BucketIt it);
  uint32_t swap(uint32_t from, uint32_t to);
  uint32_t add_number(decimal_t decimal);
  uint32_t shift_bits(uint32_t target, BucketIt start_it, int32_t len);
  uint32_t r_extend(uint32_t target, uint32_t amount);
  uint32_t r_extend(uint32_t target, uint32_t amount, uint32_t prev);
  uint32_t extend(uint32_t target, uint32_t amount);
  uint32_t extend(uint32_t target, uint32_t amount, uint32_t next);
  uint32_t rebalance(uint32_t min_distrib);
  uint32_t steal_expand(uint32_t target, uint32_t max_expand);

  template<bool invert=false, class F>
  bucket_stat_t select_bigger(F) const;
  template<class F>
  uint32_t select_first(F) const;
  template<class F, class R>
  R accumulate(F, R) const;

  uint32_t capacity(uint32_t target) const;
  uint32_t available(uint32_t target) const;
  uint32_t byte_len(uint32_t target) const;
  uint32_t prev_contiguous(uint32_t target) const;
  uint32_t next_contiguous(uint32_t target) const;
  StatBuckets calculate_stats() const;

  uint32_t _swap(uint32_t from, uint32_t to);
  uint32_t _r_extend(uint32_t target, uint32_t amount);
	uint32_t _add_number(decimal_t decimal);
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

Buckets build_buckets();
std::vector<decimal_t> generate_input(uint32_t len, uint32_t max_value);

uint32_t make_ov_error(uint32_t ov_count);
uint32_t overflow_count(uint32_t ov_error);

uint32_t write_compressed(uint32_t number, uint8_t* start, uint8_t bit_offset, uint8_t* end);
comp_int_t compress(uint32_t number);
size_t compress_len(uint32_t number);
size_t compress_len(comp_int_t comp);
int_len_t decompress(uint8_t* start, uint8_t bit_offset);
uint32_t decompress(comp_int_t comp);

Buckets order_numbers_into_buckets(const std::vector<decimal_t>& input);
uint32_t add_rebalance_if_needed(Buckets& buckets, decimal_t decimal);

using strategy_t = uint32_t(*)(Buckets&, decimal_t);
uint32_t add_rebalance_strategy_0(Buckets& buckets, decimal_t decimal);
uint32_t add_rebalance_strategy_1(Buckets& buckets, decimal_t decimal);
uint32_t add_rebalance_strategy_2(Buckets& buckets, decimal_t decimal);
uint32_t add_rebalance_strategy_3(Buckets& buckets, decimal_t decimal);
uint32_t add_rebalance_strategy_4(Buckets& buckets, decimal_t decimal);

bucket_int_t decimal_to_bucket_int(decimal_t decimal);
decimal_t bucket_int_to_decimal(bucket_int_t bucket_int);
int32_t operator-(uint8_t *lhs, const BucketIt& rhs);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<bool invert, class F>
bucket_stat_t Buckets::select_bigger(F selector) const {
  bucket_stat_t biggest = {0, selector(0)};
  for(uint32_t i=0; i < bucket_len; ++i) {
    double quant = selector(i); 
    if(invert && quant < biggest.second) 
      biggest = std::make_pair(i, quant);
    if(!invert && quant > biggest.second) 
      biggest = std::make_pair(i, quant);
  }
  return biggest;
}

template<class F>
uint32_t Buckets::select_first(F selector) const {
  for(uint32_t i=0; i < bucket_len; ++i) {
    if(selector(i)) return i;
  }
  return bucket_len;
}

template<class F, class R>
R Buckets::accumulate(F selector, R init) const {
  for(uint32_t i=0; i < bucket_len; ++i) {
    init += selector(i);
  }
  return init;
}

