#include "1m_sorting.hpp"
#include "1m_sorting_print.hpp"
#include <algorithm>
#include <functional>
#include <iostream>
#include <numeric>
#include <cmath>

using namespace std;

BucketIt::reference BucketIt::operator *() {
  auto int_len = decompress(start, bit_offset);
  MY_ASSERT(8*(end - start) >= bit_offset+int_len.second);
  value = int_len.first;
  return value;
}

int_len_t BucketIt::get_and_advance() {
  auto int_len = decompress(start, bit_offset);
  start += (bit_offset+int_len.second) / 8;
  bit_offset = (bit_offset+int_len.second) % 8;
  MY_ASSERT(start < end || (start == end && bit_offset == 0));
  return int_len;
}

void BucketIt::shift_bits(int32_t len) {
  if(len < 0) {
    start += len/8 + ((len % 8) + bit_offset < 0 ? -1 : 0);
    bit_offset = (8 + bit_offset + (len%8)) % 8;
  }
  else if(len > 0) {
    start += (bit_offset + len) / 8;
    bit_offset = (bit_offset + len) % 8;
  }
  MY_ASSERT(start < end || (start == end && bit_offset == 0));
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
  MY_ASSERT(end == rhs.end);
  return (start == rhs.start) && (bit_offset == rhs.bit_offset);
}

bool BucketIt::operator!=(const BucketIt& rhs) const {
  MY_ASSERT(end == rhs.end);
  return (start != rhs.start) || (bit_offset != rhs.bit_offset);
}

int32_t BucketIt::operator-(const BucketIt& rhs) const {
  MY_ASSERT(end == rhs.end);
  return 8 * (start - rhs.start) + (bit_offset - rhs.bit_offset);
}

bool BucketIt::operator<(const BucketIt& rhs) const {
  MY_ASSERT(end == rhs.end);
  return (start < rhs.start) || (start == rhs.start && bit_offset < rhs.bit_offset);
}

bool BucketIt::operator<=(const BucketIt& rhs) const {
  return (*this < rhs) || (*this == rhs);
}

int32_t operator-(uint8_t* lhs, const BucketIt& rhs) {
  return 8 * (lhs - rhs.start) - rhs.bit_offset;
}

ItContainer<BucketIt> Buckets::at(uint32_t idx) const {
  auto it_start = begin(idx);
  auto it_end = end(idx);
  ItContainer<BucketIt> b { it_start, it_end };
  return b;
}

ItContainer<GlobalIt> Buckets::global_at() const {
  auto it_start = global_begin();
  auto it_end = global_end();
  ItContainer<GlobalIt> b { it_start, it_end };
  return b;
}

BucketIt Buckets::begin(uint32_t idx) const {
  uint8_t *start = starts[idx];
  uint8_t *end = ends[idx];
  BucketIt it {start, end, 0};
  MY_ASSERT(it.start >= buffer.data() && it.start <= it.end);
  MY_ASSERT(it.end <= buffer.data() + buffer.size());
  return it;
}

BucketIt Buckets::end(uint32_t idx) const {
  uint8_t *start = starts[idx];
  uint8_t *end = ends[idx];
  BucketIt it {start + lens[idx]/8, end, static_cast<uint8_t>(lens[idx]%8)};
  MY_ASSERT(it.start >= buffer.data() && it.start <= it.end);
  MY_ASSERT(it.end <= buffer.data() + buffer.size());
  return it;
}

GlobalIt Buckets::global_begin() const {
  auto it = std::upper_bound(lens.begin(), lens.end(), 0);
  uint32_t idx = (uint32_t)std::distance(lens.begin(), it);
  if(idx == bucket_len)
    return GlobalIt{this, idx};

  auto global_it = GlobalIt{this, idx, begin(idx)};
  global_it.value = bucket_int_to_number(make_pair(idx, *global_it.internal_it));
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
  MY_ASSERT((int32_t)avail > -1);
  return avail;
}

uint32_t Buckets::byte_len(uint32_t target) const {
  return lens[target] / 8 + (lens[target] % 8 ? 1:0);
}

StatBuckets Buckets::calculate_stats() const {
  uint32_t item_count = 0;
  auto tot_avail_kb = Buckets::accumulate([this](uint32_t i) { return available(i); }, 0.0) / 8192;
  auto avg_cap_byte = Buckets::accumulate([this](uint32_t i) { return capacity(i); }, 0.0) / (8*bucket_len);
  auto avg_avail_byte = 1024.0 * tot_avail_kb / bucket_len;
  auto std_avail_byte = 0.0, std_cap_byte = 0.0;
  StatBuckets::Histo len_histo, val_histo;

  for(uint32_t i=0; i < bucket_len; ++i) {
    auto cap_byte = 1.0*capacity(i) / 8;
    auto avail_byte = 1.0*available(i) / 8;
    std_cap_byte   += (avg_cap_byte - cap_byte) * (avg_cap_byte - cap_byte); 
    std_avail_byte += (avg_avail_byte - avail_byte) * (avg_avail_byte - avail_byte); 
    for(auto val : at(i)) {
      item_count += 1;
      ++len_histo[compress_len(val)];
      if(val < comp_max_1)
        ++val_histo[val];
      else if(val < comp_max_2)
        ++val_histo[comp_max_1 + val/comp_max_1];
      else
        ++val_histo[comp_max_2 + val/comp_max_2];
    }
  }
  
  StatBuckets stats = {
    item_count,
    select_bigger<true>([this](uint32_t i) { return 1.0*capacity(i)/8192; }), // min_cap_kb
    select_bigger<true>([this](uint32_t i) { return 1.0*available(i)/8; }),   // min_avail_byte
    select_bigger([this](uint32_t i) { return 1.0*capacity(i)/8192; }),       // max_cap_kb
    select_bigger([this](uint32_t i) { return 1.0*available(i)/8; }),         // max_avail_kb
    avg_cap_byte,
    avg_avail_byte,
    std::sqrt(std_cap_byte / buffer_len),
    std::sqrt(std_avail_byte / bucket_len),
    std::accumulate(lens.begin(), lens.end(), 0.0) / 8192, // tot_len_kb
    tot_avail_kb,
    std::move(len_histo),
    std::move(val_histo),
  };
  return stats;
}

void Buckets::update(uint32_t idx, BucketIt it) {
  uint8_t *start = starts[idx];
  lens[idx] = 8 * (it.start - start) + it.bit_offset;
  MY_ASSERT(lens[idx] <= 8 *(ends[idx] - start));
}

void Buckets::clear() {
  for(uint32_t i=0; i<bucket_len; ++i) {
    starts[i] = buffer.data() + i * (buffer_len/bucket_len);
    ends[i] = buffer.data() + (1+i) * (buffer_len/bucket_len);
    lens[i] = 0;
  }
  ends[bucket_len-1] = buffer.data() + buffer_len;
}

uint32_t Buckets::r_extend(uint32_t target, uint32_t amount) {
  uint32_t prev_idx = prev_contiguous(target);
  return r_extend(target, amount, prev_idx);
}

uint32_t Buckets::r_extend(uint32_t target, uint32_t amount, uint32_t prev_idx) {
  MY_ASSERT(amount && prev_idx == prev_contiguous(target));
  if (prev_idx == bucket_len)
    return make_ov_error(amount);

  uint32_t avail = available(prev_idx) / 8;
  uint32_t cap   = capacity(prev_idx) / 8;
  if(avail < amount || cap == amount)
    return make_ov_error(cap == avail ? amount : amount - avail);

  uint8_t* new_start = starts[target] - amount;
  uint8_t* end_copy = starts[target] + byte_len(target);
  std::copy(starts[target], end_copy, new_start);

  starts[target] = new_start;
  ends[prev_idx] = new_start;
  return amount * 8;
}

uint32_t Buckets::extend(uint32_t target, uint32_t amount) {
  uint32_t next_idx = next_contiguous(target);
  return extend(target, amount, next_idx);
}

uint32_t Buckets::extend(uint32_t target, uint32_t amount, uint32_t next_idx) {
  MY_ASSERT(amount && next_idx == next_contiguous(target));
  if (next_idx == bucket_len)
    return make_ov_error(amount);

  uint32_t avail = available(next_idx) / 8;
  uint32_t cap   = capacity(next_idx) / 8;
  if(avail < amount || cap == amount)
    return make_ov_error(cap == avail ? amount : amount - avail);

  uint8_t* new_start = starts[next_idx] + amount;
  uint8_t* end_copy = starts[next_idx] + byte_len(next_idx);
  std::copy_backward(starts[next_idx], end_copy, end_copy + amount);

  starts[next_idx] = new_start;
  ends[target] = new_start;
  return amount * 8;
}

uint32_t Buckets::swap(uint32_t from, uint32_t to) {
  MY_ASSERT(from != to);
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

uint32_t Buckets::shift_bits(uint32_t target, BucketIt start_it, int32_t extra_len) {
  auto bucket_end = end(target);
  MY_ASSERT(start_it <= bucket_end && start_it.start >= starts[target]);
  int32_t ov_count = extra_len < 0 ? 
    -extra_len - (start_it - begin(target))
    : extra_len - available(target);
  if(ov_count > 0)
    return make_ov_error(ov_count);

  if(extra_len > 0 && start_it != bucket_end) {
    uint32_t move = extra_len / 8;
    uint32_t shft = extra_len % 8;
    uint8_t* end  = bucket_end.start;
    uint8_t* strt = start_it.start;

    MY_ASSERT(end+move <= ends[target]);
    if ((bucket_end.bit_offset + shft) > 8)
      *(end+move+1) = *end >> (8-shft);
    if ((bucket_end.bit_offset + shft) > 0)
      *(end+move) = *end << shft;
    while(strt <= --end) {
      *(end+move+1) |= *end >> (8-shft);
      *(end+move) = *end << shft;
    }
    MY_ASSERT(end+1 >= starts[target]);
  }
  else if(extra_len < 0) {
    uint32_t move = -extra_len / 8;
    uint32_t shft = -extra_len % 8;
    uint8_t* end  = bucket_end.start;
    uint8_t* strt = start_it.start;

    MY_ASSERT(strt-move-1 >= starts[target]);
    if (start_it.bit_offset < shft)
      *(strt-move-1) |= *strt << (8-shft);
    *(strt-move) = *strt >> shft;
    while(++strt <= end) {
      *(strt-move-1) |= *strt << (8-shft);
      *(strt-move) = *strt >> shft;
    }
    MY_ASSERT(strt-1 <= ends[target]);
  }

  bucket_end.shift_bits(extra_len);
  update(target, bucket_end);
  return (extra_len < 0 ? 0 : extra_len);
}

uint32_t Buckets::add_number(uint32_t number) {
  auto bucket_int = uint32_to_bucket_int(number);
  auto bucket_end = end(bucket_int.first);
  uint32_t sum = 0;

  auto insert_it = find_if(begin(bucket_int.first), bucket_end,
    [&](uint32_t n) { sum += n; return (bucket_int.second < sum ? 1 : 0); });

  if (insert_it == bucket_end) {
    uint32_t result = insert_it.write_and_advance(bucket_int.second - sum);
    update(bucket_int.first, insert_it);
    return result;
  }
  else { // begin : insert int the middle
    auto start_it = insert_it;
    auto int_len = start_it.get_and_advance();
    uint32_t delta = bucket_int.second - (sum - int_len.first);
    uint32_t next = sum - bucket_int.second; 
    int32_t extra_len = compress_len(delta) + compress_len(next) - int_len.second;
    MY_ASSERT(delta < sum && next <= sum && extra_len <= (int32_t)(2*comp_len_4));

    uint32_t ok = shift_bits(bucket_int.first, start_it, extra_len);
    if (!overflow_count(ok)) {
      ok = insert_it.write_and_advance(delta);
      MY_ASSERT(!overflow_count(ok));
      ok = insert_it.write_and_advance(next);
      MY_ASSERT(!overflow_count(ok));
      return (extra_len < 0 ? 0 : extra_len);
    }
    return ok;
  } // end: insert in the middle
}

uint32_t Buckets::rebalance(uint32_t min_distrib) {
  int32_t extend_by = 0;
  auto current = std::find(starts.begin(), starts.end(), buffer.data());
  auto next = current;
  MY_ASSERT(current != starts.end());

  while(true) {
    uint32_t idx = std::distance(starts.begin(), current);
    next = std::find(starts.begin(), starts.end(), ends[idx]);
    if(next == starts.end())
      break;

    // we use max to avoid collapsing start with end 
    extend_by = (ends[idx] - starts[idx]) - std::max(1u, byte_len(idx));
    MY_ASSERT(extend_by >= 0);

    if(extend_by) {
      uint32_t next_idx = std::distance(starts.begin(), next);
      uint32_t ok = r_extend(next_idx, extend_by, idx);
      MY_ASSERT(!overflow_count(ok));
    }
    current = next;
  }

  uint32_t distribute = std::max((size_t)min_distrib, extend_by / bucket_len);
  current = std::find(ends.begin(), ends.end(), buffer.data()+buffer_len);
  next = current;
  MY_ASSERT(current != ends.end());

  while(distribute) {
    uint32_t idx = std::distance(ends.begin(), current);
    next = std::find(ends.begin(), ends.end(), starts[idx]);
    extend_by = (ends[idx] - starts[idx]) - byte_len(idx) - distribute;
    if(next == ends.end() || extend_by < 1)
      break;

    uint32_t next_idx = std::distance(ends.begin(), next);
    uint32_t ok = extend(next_idx, extend_by, idx);
    MY_ASSERT(!overflow_count(ok));
    current = next;
  }
  return distribute;
}

uint32_t Buckets::steal_expand(uint32_t target, uint32_t max_expand) {
  const int32_t steal = 1;
  int32_t extend_left = 0, extend_right = 0;
  auto current = std::find(ends.begin(), ends.end(), buffer.data()+buffer_len);
  auto next = current;
  MY_ASSERT(current != ends.end());
  uint32_t idx = std::distance(ends.begin(), current);
  int32_t margin = (ends[idx] - starts[idx]) - byte_len(idx);

  while(true) {
    extend_right += (margin > steal ? steal : 0);
    next = std::find(ends.begin(), ends.end(), starts[idx]);
    MY_ASSERT(next != ends.end());
    uint32_t next_idx = std::distance(ends.begin(), next);
    int32_t next_margin = (ends[next_idx] - starts[next_idx]) - byte_len(next_idx);

    if(extend_right) {
      uint32_t ok = extend(next_idx, extend_right, idx);
      MY_ASSERT(!overflow_count(ok));
    }
    if(next_idx == target)
      break;
    current = next;
    idx = next_idx;
    margin = next_margin;
  }

  if((uint32_t)extend_right >= max_expand)
    return extend_right;

  current = std::find(starts.begin(), starts.end(), buffer.data());
  next = current;
  MY_ASSERT(current != starts.end());
  idx = std::distance(starts.begin(), current);
  margin = (ends[idx] - starts[idx]) - byte_len(idx);

  while(true) {
    extend_left += (margin > steal ? steal : 0);
    next = std::find(starts.begin(), starts.end(), ends[idx]);
    MY_ASSERT(next != starts.end());
    uint32_t next_idx = std::distance(starts.begin(), next);
    int32_t next_margin = (ends[next_idx] - starts[next_idx]) - byte_len(next_idx);

    if(extend_left) {
      uint32_t ok = r_extend(next_idx, extend_left, idx);
      MY_ASSERT(!overflow_count(ok));
    }
    if(next_idx == target)
      break;
    current = next;
    idx = next_idx;
    margin = next_margin;
  }
  return extend_left + extend_right;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

GlobalIt::reference GlobalIt::operator*() {
  return value;
}

GlobalIt& GlobalIt::operator++() {
  ++internal_it;
  while(internal_it == buckets->end(cur_bucket) 
        && ++cur_bucket < bucket_len) {
    internal_it = buckets->begin(cur_bucket);
    value = bucket_int_to_number(make_pair(cur_bucket, 0));
  }
  MY_ASSERT(cur_bucket == bucket_len || internal_it != buckets->end(cur_bucket));

  if(cur_bucket != bucket_len) 
    value += *internal_it;
  return *this;
}

GlobalIt GlobalIt::operator++(int) {
  GlobalIt it{*this};
  ++it;
  return it;
}

bool GlobalIt::operator==(const GlobalIt& rhs) const {
  return (cur_bucket == bucket_len && cur_bucket == rhs.cur_bucket)
    || (cur_bucket == rhs.cur_bucket && internal_it == rhs.internal_it);
}

bool GlobalIt::operator!=(const GlobalIt& rhs) const {
  return !(*this == rhs);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

Buckets build_buckets() {
  Buckets buckets;
  buckets.buffer.resize(buffer_len, 0);
  buckets.starts.resize(bucket_len, nullptr);
  buckets.ends.resize  (bucket_len, nullptr);
  buckets.lens.resize  (bucket_len, 0);
  buckets.clear();
  return buckets;
}

uint32_t make_ov_error(uint32_t ov_count) {
  return -ov_count;
}

uint32_t overflow_count(uint32_t ov_error) {
  return (ov_error >> 31) ? -ov_error : 0; 
}

bucket_int_t uint32_to_bucket_int(uint32_t number) {
  bucket_int_t bucket_int;
  bucket_int.first  = number / bucket_val_mask;
  bucket_int.second = number % bucket_val_mask;
  return bucket_int;
}

uint32_t bucket_int_to_number(bucket_int_t bucket_int) {
  return bucket_int.first * bucket_val_mask + bucket_int.second;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

uint32_t write_compressed(uint32_t number, uint8_t* start, uint8_t bit_offset, uint8_t* end) {
  MY_ASSERT(bit_offset < 8);
  const auto comp = compress(number);
  const uint8_t comp_len = compress_len_by_prefix(comp[0]);
  const int32_t extra_len = bit_offset + comp_len - 8*(end - start);
  const uint8_t mask = (1 << bit_offset) - 1;

  if(extra_len > 0)
    return make_ov_error(extra_len);

  if(bit_offset == 0 && comp_len == comp_len_1) {
    start[0] = comp[0] | (start[0] & 0x80);
    return comp_len;
  }
  if(bit_offset == 1 && comp_len == comp_len_1) {
    start[0] = (comp[0] << bit_offset) | (start[0] & mask);
    return comp_len;
  }
  if(bit_offset > 1 && comp_len == comp_len_1) {
    uint8_t imask = ~((1 << (bit_offset-1)) - 1);
    start[0] = (comp[0] << bit_offset) | (start[0] & mask);
    start[1] = (comp[0] >> (8-bit_offset)) | (start[1] & imask);
    return comp_len;
  }

  if(bit_offset == 0 && comp_len == comp_len_2) {
    start[0] = comp[0];
    start[1] = comp[1] | (start[1] & 0xc0);
    return comp_len;
  }
  if(bit_offset < 3 && comp_len == comp_len_2) {
    uint8_t scrap = (start[1] & 0x80) << (bit_offset - 1);
    start[0] = (comp[0] << bit_offset) | (start[0] & mask);
    start[1] = (comp[1] << bit_offset) | (comp[0] >> (8-bit_offset)) | scrap;
    return comp_len;
  }
  if(bit_offset > 2 && comp_len == comp_len_2) {
    uint8_t iimask = ~((1 << (bit_offset-2)) - 1);
    start[0] = (comp[0] << bit_offset)     | (start[0] & mask);
    start[1] = (comp[1] << bit_offset)     | (comp[0] >> (8-bit_offset));
    start[2] = (comp[1] >> (8-bit_offset)) | (start[2] & iimask);
    return comp_len;
  }

  if(bit_offset == 0) {
    start[0] = comp[0];
    start[1] = comp[1];
    start[2] = comp[2];
    start[3] = comp[3];
    if(comp_len == comp_len_3)
      return comp_len;
    start[4] = comp[4];
    return comp_len;
  }
  if(bit_offset != 0) {
    start[0] = (comp[0] << bit_offset)     | (start[0] & mask);
    start[1] = (comp[1] << bit_offset)     | (comp[0] >> (8-bit_offset));
    start[2] = (comp[2] << bit_offset)     | (comp[1] >> (8-bit_offset));
    start[3] = (comp[3] << bit_offset)     | (comp[2] >> (8-bit_offset));
    if(comp_len == comp_len_3) {
      start[4] = (comp[3] >> (8-bit_offset)) | (start[4] & ~mask);
      return comp_len;
    }
    start[4] = (comp[4] << bit_offset)     | (comp[3] >> (8-bit_offset));
    start[5] = (comp[4] >> (8-bit_offset)) | (start[5] & ~mask);
    return comp_len;
  }
  MY_ASSERT(false);
  return 0;
}

comp_int_t compress(uint32_t number) {
  MY_ASSERT(number < comp_max_4);
  comp_int_t comp;

  if(number < comp_max_1) {
    comp[0] = number;
  }
  else if(number < comp_max_2) {
    uint8_t quot = (number - comp_max_1 + 1) % 0x80;
    comp[0] = (number - comp_max_1 + 1)/0x80 + comp_max_1 + (quot << 7);
    comp[1] = (quot >> 1);
  }
  else if(number < comp_max_3) {
    comp[0] = 0x7f;
    comp[1] = number & 0xff;
    comp[2] = (number >> 8) & 0xff;
    comp[3] = (number >> 16) & 0xff;
  }
  else {
    comp[0] = 0xff;
    comp[1] = number & 0xff;
    comp[2] = (number >> 8) & 0xff;
    comp[3] = (number >> 16) & 0xff;
    comp[4] = (number >> 24) & 0xff;
  }
  return comp;
}

size_t compress_len(uint32_t number) {
  if(number < comp_max_1)
    return comp_len_1;
  if(number < comp_max_2)
    return comp_len_2;
  if(number < comp_max_3)
    return comp_len_3;
  return comp_len_4;
}

size_t compress_len_by_prefix(uint8_t comp_0) {
  uint8_t prefix = (comp_0 & 0x7f);
  if(prefix < comp_max_1)
    return comp_len_1;
  if(prefix < 0x7f)
    return comp_len_2;
  if(comp_0 < 0xff)
    return comp_len_3;
  return comp_len_4;
}

int_len_t decompress(uint8_t* start, uint8_t bit_offset) {
  comp_int_t comp;

  comp[0] = (start[0] >> bit_offset);
  if(bit_offset)
    comp[0] |= (start[1] << (8-bit_offset));

  uint8_t comp_len = compress_len_by_prefix(comp[0]);
  if(comp_len == comp_len_1)
    return make_pair(decompress(comp), comp_len);

  comp[1] = (start[1] >> bit_offset);
  if(bit_offset)
    comp[1] |= (start[2] << (8-bit_offset));
  if(comp_len == comp_len_2)
    return make_pair(decompress(comp), comp_len);

  comp[2] = (start[2] >> bit_offset);
  comp[3] = (start[3] >> bit_offset);
  if(bit_offset) {
    comp[2] |= (start[3] << (8-bit_offset));
    comp[3] |= (start[4] << (8-bit_offset));
  }
  if(comp_len == comp_len_3)
    return make_pair(decompress(comp), comp_len);

  comp[4] = (start[4] >> bit_offset);
  if(bit_offset)
    comp[4] |= (start[5] << (8-bit_offset));
  return make_pair(decompress(comp), comp_len);
}

uint32_t decompress(comp_int_t comp) {
  uint8_t comp_len = compress_len_by_prefix(comp[0]);

  if(comp_len == comp_len_1)
    return comp[0] & 0x7f;
  if(comp_len == comp_len_2) {
    uint8_t a1 = comp[0] & 0x7f;
    uint8_t a0 = ((comp[0] >> 7) | (comp[1] << 1)) & 0x7f;
    return comp_max_1 - 1 + 0x80*(a1 - comp_max_1) + a0;
  }
  if(comp_len == comp_len_3)
    return comp[1] + 0x100 * comp[2] + 0x10000 * comp[3];
  return comp[1] + 0x100 * comp[2] + 0x10000 * comp[3] + 0x1000000 * comp[4];
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

uint32_t add_rebalance_if_needed(Buckets& buckets, uint32_t number) {
  const static strategy_t add_strats[] = {
    add_rebalance_strategy_0,
    add_rebalance_strategy_1,
    add_rebalance_strategy_2,
    add_rebalance_strategy_3,
  };
  uint32_t ok = make_ov_error(1);

  for(auto strat : add_strats) {
    ok = strat(buckets, number);
    if(!overflow_count(ok)) break;
  }
  if(overflow_count(ok)) {
    STRAT_LOG(5, "before balance : " << my_format(buckets));
    buckets.rebalance(safe_bucket_inc);
    STRAT_LOG(5, "after balance : stats=" << print_stats(buckets.calculate_stats()));

    ok = add_rebalance_strategy_0(buckets, number);
    if(overflow_count(ok)) 
      ok = add_rebalance_strategy_4(buckets, number);
  }
  return ok;
}

// strategy 0 : just try to add
uint32_t add_rebalance_strategy_0(Buckets& buckets, uint32_t number) {
  uint32_t ok = buckets.add_number(number);
  //STRAT_LOG(0, "number=" << my_format(number) << " ov_count=" << ok);
  return ok;
}

// strategy 1 : extend (in both directions) by the preferred length to make space
uint32_t add_rebalance_strategy_1(Buckets& buckets, uint32_t number) {
  auto bucket_int = uint32_to_bucket_int(number);

  uint32_t ok = buckets.r_extend(bucket_int.first, safe_bucket_inc);
  STRAT_LOG(1, "number=" << my_format(number) << " ov_count=" << ok << " bucket_int=" << my_format(bucket_int) << endl
    << " stats=" << my_format(buckets, bucket_int.first) << " cont_stats=" << my_format(buckets, buckets.prev_contiguous(bucket_int.first)));
  if(overflow_count(ok)) {
    ok = buckets.extend(bucket_int.first, safe_bucket_inc);
    STRAT_LOG(1, "number=" << my_format(number) << " ov_count=" << ok << " bucket_int=" << my_format(bucket_int) << endl
      << " stats=" << my_format(buckets, bucket_int.first) << " cont_stats=" << my_format(buckets, buckets.next_contiguous(bucket_int.first)));
  }
  if(!overflow_count(ok)) {
    ok = buckets.add_number(number);
    MY_ASSERT(!overflow_count(ok));
  }
  return ok;
}

// strategy 2 : swap target bucket with the one with more space
uint32_t add_rebalance_strategy_2(Buckets& buckets, uint32_t number) {
  auto bucket_int = uint32_to_bucket_int(number);
  uint32_t ok = 0;
  uint32_t target_len = buckets.lens[bucket_int.first] + 8*safe_bucket_inc;
  uint32_t target_cap = buckets.capacity(bucket_int.first);
  uint32_t swap_idx = buckets.select_first([&](uint32_t i) { 
    return i != bucket_int.first && buckets.capacity(i) >= target_len && target_cap >= buckets.lens[i];
  });
  
  if(swap_idx == bucket_len)
    ok = make_ov_error(1);
  else {
    ok = buckets.swap(swap_idx, bucket_int.first);
    MY_ASSERT(!overflow_count(ok));
    ok = buckets.add_number(number);
    MY_ASSERT(!overflow_count(ok));
  }
  STRAT_LOG(2, "number=" << my_format(number) << " ov_count=" << ok
      << " bucket_int=" << my_format(bucket_int) << " swap_idx=" << swap_idx << endl
      << " stats=" << my_format(buckets, bucket_int.first) << " swap_stats=" << my_format(buckets, swap_idx));
  return ok;
}

// strategy 3 : swap any contiguous bucket with one with more space and extend
uint32_t add_rebalance_strategy_3(Buckets& buckets, uint32_t number) {
  using extend_t = function<uint32_t(uint32_t, uint32_t, uint32_t)>;
  uint32_t ok = make_ov_error(1);
  auto bucket_int = uint32_to_bucket_int(number);
  
  auto side_strat = [&buckets, &number, &bucket_int, &ok] (uint32_t cont_idx, extend_t func) {
    uint32_t target_len = buckets.lens[cont_idx] - 8*safe_bucket_inc;
    uint32_t swap_idx = buckets.select_first([&](uint32_t i) { 
      return i != cont_idx && buckets.lens[i] <= target_len && buckets.lens[cont_idx] <= buckets.capacity(i);
    });
    
    if(swap_idx == bucket_len || (int32_t)target_len < 0)
      ok = make_ov_error(1);
    else {
      ok = buckets.swap(swap_idx, cont_idx);
      MY_ASSERT(!overflow_count(ok));
      ok = func(bucket_int.first, safe_bucket_inc, swap_idx);
      MY_ASSERT(!overflow_count(ok));
      ok = buckets.add_number(number);
      MY_ASSERT(!overflow_count(ok));
    }
    STRAT_LOG(3, "number=" << my_format(number) << " ov_count=" << ok
        << " bucket_int=" << my_format(bucket_int) << " cont_idx=" << cont_idx << " swap_idx=" << swap_idx << endl
        << " stats=" << my_format(buckets, bucket_int.first) << " swap_stats=" << my_format(buckets, swap_idx));
  };

  uint32_t cont_idx = buckets.prev_contiguous(bucket_int.first);
  if (cont_idx != bucket_len)
    side_strat(cont_idx, [&buckets](uint32_t t, uint32_t a, uint32_t c) { return buckets.r_extend(t, a, c); });
  if(overflow_count(ok)) {
    cont_idx = buckets.next_contiguous(bucket_int.first);
    if (cont_idx != bucket_len)
      side_strat(cont_idx, [&buckets](uint32_t t, uint32_t a, uint32_t c) { return buckets.extend(t, a, c); });
  }
  return ok;
}

// strategy 4 : steal 1 byte from all possible buckets until there is enough space
uint32_t add_rebalance_strategy_4(Buckets& buckets, uint32_t number) {
  auto bucket_int = uint32_to_bucket_int(number);
  buckets.steal_expand(bucket_int.first, safe_bucket_inc);
  uint32_t ok = buckets.add_number(number);
  STRAT_LOG(4, "number=" << my_format(number) << " ov_count=" << ok
      << " bucket_int=" << my_format(bucket_int) << endl
      << " stats=" << my_format(buckets, bucket_int.first));
  return ok;
}

Buckets order_numbers_into_buckets(const vector<uint32_t>& input) {
  Buckets buckets = build_buckets();
  uint32_t count = 0;
  for(auto number : input) {
    uint32_t ok = add_rebalance_if_needed(buckets, number);
    if(overflow_count(ok)) {
      LOG("failed to add : " << my_format(number));
      LOG("stats : " << print_stats(buckets.calculate_stats()));
      LOG("raw : " << my_format(buckets));
      assert(false);
    }
    //if(count % (input_len/20) == 0)
    //  LOG("buckets stats (" << count << ") = " << print_stats(buckets.calculate_stats()));
    ++count;
  }
  return buckets;
}

