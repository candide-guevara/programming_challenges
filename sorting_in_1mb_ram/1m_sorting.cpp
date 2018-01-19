#include "1m_sorting.hpp"
#include "1m_sorting_print.hpp"
#include <algorithm>
#include <deque>
#include <functional>
#include <iostream>
#include <numeric>
#include <cmath>
#pragma GCC diagnostic ignored "-Wunused-variable"

using namespace std;

BucketIt::reference BucketIt::operator *() {
  auto int_len = decompress(start, bit_offset);
  assert(8*(end - start) >= bit_offset+int_len.second);
  value = int_len.first;
  return value;
}

int_len_t BucketIt::get_and_advance() {
  auto int_len = decompress(start, bit_offset);
  start += (bit_offset+int_len.second) / 8;
  bit_offset = (bit_offset+int_len.second) % 8;
  assert(start <= end);
  return int_len;
}

uint32_t BucketIt::write_and_advance(uint32_t number) {
  auto next_offset = write_compressed(number, start, bit_offset, end);
  if (!overflow_count(next_offset)) {
    start += (bit_offset+next_offset) / 8;
    bit_offset = (bit_offset+next_offset) % 8;
  }
  return next_offset;
}

BucketIt& BucketIt::operator ++() {
  get_and_advance();
  return *this;
}

BucketIt BucketIt::operator ++(int) {
  BucketIt it{*this};
  get_and_advance();
  return it;
}

bool BucketIt::operator==(const BucketIt& rhs) const {
  assert(end == rhs.end);
  return (start == rhs.start) && (bit_offset == rhs.bit_offset);
}

bool BucketIt::operator!=(const BucketIt& rhs) const {
  assert(end == rhs.end);
  return (start != rhs.start) || (bit_offset != rhs.bit_offset);
}

int32_t BucketIt::operator-(const BucketIt& rhs) const {
  assert(end == rhs.end);
  return 8 * (start - rhs.start) + (bit_offset - rhs.bit_offset);
}

int32_t operator-(uint8_t* lhs, const BucketIt& rhs) {
  return 8 * (lhs - rhs.start) - rhs.bit_offset;
}

ItContainer<BucketIt> Buckets::at(uint32_t idx) {
  auto it_start = begin(idx);
  auto it_end = end(idx);
  ItContainer<BucketIt> b { it_start, it_end };
  return b;
}

ItContainer<GlobalIt> Buckets::global_at() {
  auto it_start = global_begin();
  auto it_end = global_end();
  ItContainer<GlobalIt> b { it_start, it_end };
  return b;
}

BucketIt Buckets::begin(uint32_t idx) const {
  uint8_t *start = starts[idx];
  uint8_t *end = ends[idx];
  BucketIt it {start, end, 0};
  assert(it.start >= buffer.data() && it.start <= it.end);
  assert(it.end <= buffer.data() + buffer.size());
  return it;
}

BucketIt Buckets::end(uint32_t idx) const {
  uint8_t *start = starts[idx];
  uint8_t *end = ends[idx];
  BucketIt it {start + lens[idx]/8, end, static_cast<uint8_t>(lens[idx]%8)};
  assert(it.start >= buffer.data() && it.start <= it.end);
  assert(it.end <= buffer.data() + buffer.size());
  return it;
}

GlobalIt Buckets::global_begin() const {
  auto it = std::upper_bound(lens.begin(), lens.end(), 0);
  uint32_t idx = (uint32_t)std::distance(lens.begin(), it);
  if(idx == Buckets::bucket_len)
    return GlobalIt{this, idx};

  auto global_it = GlobalIt{this, idx, begin(idx)};
  global_it.value = decimal_to_bucket_int(make_pair(*global_it.internal_it, idx));
  return global_it;
}

GlobalIt Buckets::global_end() const {
  return GlobalIt{this, bucket_len};
}

uint32_t Buckets::prev_contiguous(uint32_t target) const {
  auto it = find(ends.begin(), ends.end(), starts[target]);
  if(it == ends.end())
    return bucket_len;
  return distance(ends.begin(), it);
}

uint32_t Buckets::next_contiguous(uint32_t target) const {
  auto it = find(starts.begin(), starts.end(), ends[target]);
  if(it == starts.end())
    return bucket_len;
  return distance(starts.begin(), it);
}

uint32_t Buckets::capacity(uint32_t target) const {
  return 8*(ends[target] - starts[target]);
}

uint32_t Buckets::available(uint32_t target) const {
  uint32_t avail =  8*(ends[target] - starts[target]) - lens[target];
  assert((int32_t)avail > -1);
  return avail;
}

uint32_t Buckets::byte_len(uint32_t target) const {
  return lens[target] / 8 + (lens[target] % 8 ? 1:0);
}

StatBuckets Buckets::calculate_stats() const {
  auto tot_avail = Buckets::accumulate([this](uint32_t i) { return available(i); }, 0u);
  auto avg_cap = Buckets::accumulate([this](uint32_t i) { return capacity(i); }, 0.0) / bucket_len;
  auto avg_avail = (double)tot_avail / bucket_len;
  auto std_cap = Buckets::accumulate([this, avg_cap](uint32_t i) { 
    return (avg_cap - capacity(i))*(avg_cap - capacity(i)); 
  }, 0.0) / bucket_len;
  auto std_avail = Buckets::accumulate([this, avg_avail](uint32_t i) { 
    return (avg_avail - available(i))*(avg_avail - available(i)); 
  }, 0.0) / bucket_len;
  
  StatBuckets stats = {
    select_bigger<true>([this](uint32_t i) { return capacity(i); }),
    select_bigger<true>([this](uint32_t i) { return available(i); }),
    select_bigger([this](uint32_t i) { return capacity(i); }),
    select_bigger([this](uint32_t i) { return available(i); }),
    avg_cap,
    avg_avail,
    std::sqrt(std_cap),
    std::sqrt(std_avail),
    std::accumulate(lens.begin(), lens.end(), 0u),
    tot_avail,
  };
  return stats;
}

void Buckets::update(uint32_t idx, BucketIt it) {
  uint8_t *start = starts[idx];
  lens[idx] = 8 * (it.start - start) + it.bit_offset;
  assert(lens[idx] <= 8 *(ends[idx] - start));
}

void Buckets::clear() {
  for(uint32_t i=0; i<Buckets::bucket_len; ++i) {
    starts[i] = buffer.data() + i * (Buckets::buffer_len/Buckets::bucket_len);
    ends[i] = buffer.data() + (1+i) * (Buckets::buffer_len/Buckets::bucket_len);
    lens[i] = 0;
  }
  ends[Buckets::bucket_len-1] = buffer.data() + Buckets::buffer_len;
}

uint32_t Buckets::r_extend(uint32_t target, uint32_t amount) {
  assert(amount);
  uint32_t prev_idx = prev_contiguous(target);
  if (prev_idx == Buckets::bucket_len)
    return make_ov_error(amount);

  uint32_t avail = available(prev_idx) / 8;
  if(avail <= amount)
    return make_ov_error(std::max(amount - avail, 1u));

  uint8_t* new_start = starts[target] - amount;
  uint8_t* end_copy = starts[target] + byte_len(target);
  std::copy(starts[target], end_copy, new_start);

  starts[target] = new_start;
  ends[prev_idx] = new_start;
  return amount * 8;
}

uint32_t Buckets::extend(uint32_t target, uint32_t amount) {
  assert(amount);
  uint32_t next_idx = next_contiguous(target);
  if (next_idx == Buckets::bucket_len)
    return make_ov_error(amount);

  uint32_t avail = available(next_idx) / 8;
  if(avail <= amount)
    return make_ov_error(std::max(amount - avail, 1u));

  uint8_t* new_start = starts[next_idx] + amount;
  uint8_t* end_copy = starts[next_idx] + byte_len(next_idx);
  std::copy_backward(starts[next_idx], end_copy, end_copy + amount);

  starts[next_idx] = new_start;
  ends[target] = new_start;
  return amount * 8;
}

uint32_t Buckets::swap(uint32_t from, uint32_t to) {
  assert(from != to);
  int32_t extra_len = std::min(capacity(from) - lens[to], capacity(to) - lens[from]);
  if(extra_len < 0)
    return make_ov_error(-extra_len);

  if(lens[from] > lens[to]) {
    uint8_t *cp_dest = starts[to] + byte_len(to);
    auto cp_source = std::swap_ranges(starts[to], cp_dest, starts[from]);
    std::copy(cp_source, starts[from] + byte_len(from), cp_dest);
  }
  else if(lens[to] > lens[from]) {
    uint8_t *cp_dest = starts[from] + byte_len(from);
    auto cp_source = std::swap_ranges(starts[from], cp_dest, starts[to]);
    std::copy(cp_source, starts[to] + byte_len(to), cp_dest);
  }
  else
    std::swap_ranges(starts[from], starts[from] + byte_len(from), starts[to]);

  std::swap(starts[from], starts[to]);
  std::swap(ends[from], ends[to]);
  return 0;
}

uint32_t Buckets::add_number(decimal_t decimal) {
  auto bucket_int = decimal_to_bucket_int(decimal);
  auto bucket_end = end(bucket_int.first);
  uint32_t sum = 0;
  auto insert_it = find_if(begin(bucket_int.first), bucket_end,
    [&](uint32_t n) { sum += n; return (bucket_int.second < sum ? 1 : 0); });

  if (insert_it == bucket_end) {
    uint32_t result = insert_it.write_and_advance(bucket_int.second - sum);
    update(bucket_int.first, insert_it);
    return result;
  }
  else {
    auto read_it = insert_it;
    auto int_len = read_it.get_and_advance();
    uint32_t delta = bucket_int.second - (sum - int_len.first);
    uint32_t next = sum - bucket_int.second; 
    int32_t extra_len = compress_len(delta) + compress_len(next) - int_len.second;
    int32_t ov_count = extra_len - (bucket_end.end - bucket_end);

    if(ov_count > 0)
      return make_ov_error(ov_count);

    assert(delta < sum && next <= sum && extra_len <= (int32_t)comp_len_4);
    deque<uint32_t> buffer = { delta, next };

    for(uint32_t i=2; i < safe_buf_len && read_it != bucket_end; ++i)
      buffer.push_back(read_it.get_and_advance().first);

    while(!buffer.empty()) {
      insert_it.write_and_advance(buffer.front());
      buffer.pop_front();
      if(read_it != bucket_end)
        buffer.push_back(read_it.get_and_advance().first);
    }

    update(bucket_int.first, insert_it);
    return extra_len < 0 ? 0 : extra_len;
  }
}

uint32_t Buckets::rebalance(uint32_t amount) {
  uint32_t moved = 0;
  auto current = std::find(starts.begin(), starts.end(), buffer.data());
  auto next = current;
  assert(current != starts.end());

  while(true) {
    uint32_t idx = std::distance(starts.begin(), current);
    next = std::find(starts.begin(), starts.end(), ends[idx]);
    if(next == starts.end())
      break;

    int32_t extend_by = (ends[idx] - starts[idx]) - byte_len(idx) - amount;
    extend_by = extend_by < 0 ? 0 : extend_by;

    if(extend_by) {
      uint32_t next_idx = std::distance(starts.begin(), next);
      uint32_t ok = r_extend(next_idx, extend_by);
      assert(!overflow_count(ok));
      moved += extend_by;
    }
    current = next;
  }
  return moved;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

GlobalIt::reference GlobalIt::operator*() {
  return value;
}

GlobalIt& GlobalIt::operator++() {
  ++internal_it;
  while(internal_it == buckets->end(cur_bucket) 
        && ++cur_bucket < Buckets::bucket_len) {
    internal_it = buckets->begin(cur_bucket);
    value = bucket_int_to_decimal(make_pair(cur_bucket, 0));
  }
  assert(cur_bucket == Buckets::bucket_len || internal_it != buckets->end(cur_bucket));

  if(cur_bucket != Buckets::bucket_len) 
    value.first += *internal_it;
  return *this;
}

GlobalIt GlobalIt::operator++(int) {
  GlobalIt it{*this};
  ++it;
  return it;
}

bool GlobalIt::operator==(const GlobalIt& rhs) const {
  return (cur_bucket == Buckets::bucket_len && cur_bucket == rhs.cur_bucket)
    || (cur_bucket == rhs.cur_bucket && internal_it == rhs.internal_it);
}

bool GlobalIt::operator!=(const GlobalIt& rhs) const {
  return !(*this == rhs);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

Buckets build_buckets() {
  Buckets buckets;
  buckets.buffer.resize(Buckets::buffer_len, 0);
  buckets.starts.resize(Buckets::bucket_len, nullptr);
  buckets.ends.resize  (Buckets::bucket_len, nullptr);
  buckets.lens.resize  (Buckets::bucket_len, 0);
  buckets.clear();
  return buckets;
}

uint32_t make_ov_error(uint32_t ov_count) {
  return -ov_count;
}

uint32_t overflow_count(uint32_t ov_error) {
  return (ov_error >> 31) ? -ov_error : 0; 
}

bucket_int_t decimal_to_bucket_int(decimal_t decimal) {
  bucket_int_t bucket_int;
  bucket_int.first = (decimal.second * bucket_family_len) + (decimal.first / bucket_val_mask);
  bucket_int.second = decimal.first % bucket_val_mask;

  assert(bucket_int.first < Buckets::bucket_len);
  return bucket_int;
}

decimal_t bucket_int_to_decimal(bucket_int_t bucket_int) {
  decimal_t decimal;
  decimal.first = bucket_int.second + (bucket_int.first % bucket_family_len) * bucket_val_mask;
  decimal.second = bucket_int.first / bucket_family_len;

  assert(decimal.first < rand_msk && decimal.second < decimal_places);
  return decimal;
}

bool comp_decimal(decimal_t lhs, decimal_t rhs) {
  return (lhs.second < rhs.second)
         || (lhs.second == rhs.second && lhs.first < rhs.first);
}

uint32_t write_compressed(uint32_t number, uint8_t* start, uint8_t bit_offset, uint8_t* end) {
  assert(bit_offset < 8);
  auto comp = compress(number);
  int32_t extra_len = bit_offset + compress_len(comp) - 8*(end - start);

  if(extra_len > 0)
    return make_ov_error(extra_len);

  if(bit_offset == 0 && (comp[0] & 0x40) == 0) {
    start[0] = (comp[0] & 0x3f) | (start[0] & 0x80);
    return comp_len_1;
  }
  if(bit_offset == 1 && (comp[0] & 0x40) == 0) {
    start[0] = (comp[0] << 1) | (start[0] & 0x01);
    return comp_len_1;
  }
  if(bit_offset > 1 && (comp[0] & 0x40) == 0) {
    start[0] = (comp[0] << bit_offset) | (start[0] & ((1 << bit_offset) - 1));
    start[1] = (start[1] & ~((1 << (bit_offset-1)) - 1)) | ((comp[0] & 0x7f) >> (8-bit_offset));
    return comp_len_1;
  }
  if(bit_offset == 0 && (comp[1] & 0x40) == 0) {
    start[0] = comp[0];
    start[1] = (comp[1] & 0x3f) | (start[1] & 0x80);
    return comp_len_2;
  }
  if(bit_offset == 1 && (comp[1] & 0x40) == 0) {
    start[0] = (comp[0] << 1) | (start[0] & 0x01);
    start[1] = (comp[0] >> 7) | (comp[1] << 1);
    return comp_len_2;
  }
  if(bit_offset > 1 && (comp[1] & 0x40) == 0) {
    start[0] = (comp[0] << bit_offset) | (start[0] & ((1 << bit_offset) - 1));
    start[1] = (comp[0] >> (8-bit_offset)) | (comp[1] << bit_offset);
    start[2] = (start[2] & ~((1 << (bit_offset-1)) - 1)) | ((comp[1] & 0x7f) >> (8-bit_offset));
    return comp_len_2;
  }
  if(bit_offset == 0 && (comp[2] & 0x80) == 0) {
    start[0] = comp[0];
    start[1] = comp[1];
    start[2] = comp[2];
    return comp_len_3;
  }
  if(bit_offset > 0 && (comp[2] & 0x80) == 0) {
    start[0] = (comp[0] << bit_offset) | (start[0] & ((1 << bit_offset) - 1));
    start[1] = (comp[0] >> (8-bit_offset)) | (comp[1] << bit_offset);
    start[2] = (comp[1] >> (8-bit_offset)) | (comp[2] << bit_offset);
    start[3] = (start[3] & ~((1 << bit_offset) - 1)) | (comp[2] >> (8-bit_offset));
    return comp_len_3;
  }
  if(bit_offset == 0) {
    start[0] = comp[0];
    start[1] = comp[1];
    start[2] = comp[2];
    start[3] = comp[3];
    return comp_len_4;
  }
  start[0] = (comp[0] << bit_offset) | (start[0] & ((1 << bit_offset) - 1));
  start[1] = (comp[0] >> (8-bit_offset)) | (comp[1] << bit_offset);
  start[2] = (comp[1] >> (8-bit_offset)) | (comp[2] << bit_offset);
  start[3] = (comp[2] >> (8-bit_offset)) | (comp[3] << bit_offset);
  start[4] = (start[4] & ~((1 << bit_offset) - 1)) | (comp[3] >> (8-bit_offset));
  return comp_len_4;
}

comp_int_t compress(uint32_t number) {
  assert(number < comp_max_4);
  comp_int_t comp;
  int32_t biased = number - bias;
  uint32_t zigzag = (biased >> 31) ^ (biased << 1);

  if(zigzag < comp_max_1) {
    comp[0] = (zigzag & 0x3f);
  }
  else if(zigzag < comp_max_2) {
    comp[0] = (zigzag & 0x3f) | 0x40 | ((zigzag << 1) & 0x80);
    comp[1] = ((zigzag >> 7) & 0x3f);
    assert(comp[0] > 0);
  }
  else if(zigzag < comp_max_3) {
    comp[0] = (zigzag & 0x3f) | 0x40 | ((zigzag << 1) & 0x80);
    comp[1] = ((zigzag >> 7) & 0x3f) | 0x40 | ((zigzag >> 6) & 0x80);
    comp[2] = (zigzag >> 14) & 0x7f;
    assert(comp[0] > 0 && comp[1] > 0);
  }
  else {
    comp[0] = (zigzag & 0x3f) | 0x40 | ((zigzag << 1) & 0x80);
    comp[1] = ((zigzag >> 7) & 0x3f) | 0x40 | ((zigzag >> 6) & 0x80);
    comp[2] = ((zigzag >> 14) & 0x7f) | 0x80;
    comp[3] = (zigzag >> 21) & 0xff;
    assert(comp[0] > 0 && comp[1] > 0 && comp[2] > 0);
  }
  return comp;
}

size_t compress_len(uint32_t number) {
  return compress_len(compress(number));
}

size_t compress_len(comp_int_t comp) {
  if(!(comp[0] & 0x40))
    return comp_len_1;
  if(!(comp[1] & 0x40))
    return comp_len_2;
  if(!(comp[2] & 0x80))
    return comp_len_3;
  return comp_len_4;
}

int_len_t decompress(uint8_t* start, uint8_t bit_offset) {
  comp_int_t comp;
  comp[0] = (start[0] >> bit_offset);
  if(bit_offset)
    comp[0] |= (start[1] << (8-bit_offset));
  if((comp[0] & 0x40) == 0) {
    comp[0] &= 0x7f;
    return make_pair(decompress(comp), comp_len_1);
  }

  comp[1] = (start[1] >> bit_offset);
  if(bit_offset)
    comp[1] |= (start[2] << (8-bit_offset));
  if((comp[1] & 0x40) == 0) {
    comp[1] &= 0x7f;
    return make_pair(decompress(comp), comp_len_2);
  }

  comp[2] = (start[2] >> bit_offset);
  if(bit_offset)
    comp[2] |= (start[3] << (8-bit_offset));
  if((comp[2] & 0x80) == 0) {
    return make_pair(decompress(comp), comp_len_3);
  }

  comp[3] = (start[3] >> bit_offset);
  if(bit_offset)
    comp[3] |= (start[4] << (8-bit_offset));
  return make_pair(decompress(comp), comp_len_4);
}

uint32_t decompress(comp_int_t comp) {
  if((comp[0] & 0x40) == 0) {
    int32_t zigzag = comp[0] & 0x3f;
    assert((uint32_t)zigzag < comp_max_1);
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
  if((comp[1] & 0x40) == 0) {
    int32_t zigzag = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    zigzag         |= ((comp[1] >> 1) & 0x1f) << 8;
    assert((uint32_t)zigzag < comp_max_2);
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
  if((comp[2] & 0x80) == 0) {
    int32_t zigzag = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    zigzag         |= ( ((comp[1] >> 1) & 0x1f) | ((comp[1] >> 2) & 0x20) | ((comp[2] << 6) & 0xc0) ) << 8;
    zigzag         |= ( ((comp[2] >> 2) & 0x1f) ) << 16;
    assert((uint32_t)zigzag < comp_max_3);
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
  else {
    assert(comp[3] > 0);
    int32_t zigzag = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    zigzag         |= ( ((comp[1] >> 1) & 0x1f) | ((comp[1] >> 2) & 0x20) | ((comp[2] << 6) & 0xc0) ) << 8;
    zigzag         |= ( ((comp[2] >> 2) & 0x1f) | ((comp[3] << 5) & 0xe0) ) << 16;
    zigzag         |= ( ((comp[3] >> 3) & 0x1f) ) << 24;
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

uint32_t add_rebalance_if_needed(Buckets& buckets, decimal_t decimal) {
  const static strategy_t add_strats[] = {
    add_rebalance_strategy_0,
    add_rebalance_strategy_1,
    add_rebalance_strategy_2,
    add_rebalance_strategy_3,
  };
  uint32_t ok;

  for(auto strat : add_strats) {
    ok = strat(buckets, decimal);
    if(!overflow_count(ok)) break;
  }
  if(overflow_count(ok)) {
    LOG(my_format(buckets.calculate_stats()));
    uint32_t moved = buckets.rebalance(safe_buf_len);
    LOG(my_format(buckets.calculate_stats()));
    for(auto strat : add_strats) {
      ok = strat(buckets, decimal);
      if(!overflow_count(ok)) break;
    }
  }
  return ok;
}

// strategy 0 : just try to add
uint32_t add_rebalance_strategy_0(Buckets& buckets, decimal_t decimal) {
  uint32_t ok = buckets.add_number(decimal);
  STRAT_LOG(0, "decimal=" << my_format(decimal) << " ov_count=" << ok);
  return ok;
}

// strategy 1 : extend (in both directions) by the preferred length to make space
uint32_t add_rebalance_strategy_1(Buckets& buckets, decimal_t decimal) {
  auto bucket_int = decimal_to_bucket_int(decimal);

  uint32_t ok = buckets.r_extend(bucket_int.first, safe_bucket_inc);
  STRAT_LOG(1, "decimal=" << my_format(decimal) << " ov_count=" << ok << " bucket_int=" << my_format(bucket_int));
  if(overflow_count(ok)) {
    ok = buckets.extend(bucket_int.first, safe_bucket_inc);
    STRAT_LOG(1, "decimal=" << my_format(decimal) << " ov_count=" << ok << " bucket_int=" << my_format(bucket_int));
  }
  if(!overflow_count(ok)) {
    ok = buckets.add_number(decimal);
    assert(!overflow_count(ok));
  }
  return ok;
}

// strategy 2 : swap target bucket with the one with more space
uint32_t add_rebalance_strategy_2(Buckets& buckets, decimal_t decimal) {
  auto bucket_int = decimal_to_bucket_int(decimal);
  uint32_t ok = 0;
  uint32_t target_len = buckets.lens[bucket_int.first] + 8*safe_bucket_inc;
  uint32_t target_cap = buckets.capacity(bucket_int.first);
  uint32_t swap_idx = buckets.select_first([&](uint32_t i) { 
    return i != bucket_int.first && buckets.capacity(i) >= target_len && target_cap >= buckets.lens[i];
  });
  
  if(swap_idx == Buckets::bucket_len)
    ok = make_ov_error(1);
  else {
    ok = buckets.swap(swap_idx, bucket_int.first);
    assert(!overflow_count(ok));
    ok = buckets.add_number(decimal);
    assert(!overflow_count(ok));
  }
  STRAT_LOG(2, "decimal=" << my_format(decimal) << " ov_count=" << ok
      << " bucket_int=" << my_format(bucket_int) << " swap_idx=" << swap_idx);
  return ok;
}

// strategy 3 : swap any contiguous bucket with one with more space and extend
uint32_t add_rebalance_strategy_3(Buckets& buckets, decimal_t decimal) {
  uint32_t ok = make_ov_error(1);
  auto bucket_int = decimal_to_bucket_int(decimal);
  
  auto side_strat = [&buckets, &decimal, &bucket_int, &ok] (uint32_t cont_idx, function<uint32_t()> func) {
    uint32_t target_len = buckets.lens[cont_idx] - 8*safe_bucket_inc;
    uint32_t swap_idx = buckets.select_first([&](uint32_t i) { 
      return i != cont_idx && buckets.lens[i] <= target_len && buckets.lens[cont_idx] <= buckets.capacity(i);
    });
    
    if(swap_idx == Buckets::bucket_len || (int32_t)target_len < 0)
      ok = make_ov_error(1);
    else {
      ok = buckets.swap(swap_idx, cont_idx);
      assert(!overflow_count(ok));
      ok = func();
      assert(!overflow_count(ok));
      ok = buckets.add_number(decimal);
      assert(!overflow_count(ok));
    }
    STRAT_LOG(3, "decimal=" << my_format(decimal) << " ov_count=" << ok
        << " bucket_int=" << my_format(bucket_int) << " cont_idx=" << cont_idx << " swap_idx=" << swap_idx);
  };

  uint32_t cont_idx = buckets.prev_contiguous(bucket_int.first);
  if (cont_idx != Buckets::bucket_len)
    side_strat(cont_idx, [&]() { return buckets.r_extend(bucket_int.first, safe_bucket_inc); });
  if(overflow_count(ok)) {
    cont_idx = buckets.next_contiguous(bucket_int.first);
    if (cont_idx != Buckets::bucket_len)
      side_strat(cont_idx, [&]() { return buckets.extend(bucket_int.first, safe_bucket_inc); });
  }
  return ok;
}

void order_numbers_into_buckets(Buckets& buckets, const vector<decimal_t>& input) {
}

