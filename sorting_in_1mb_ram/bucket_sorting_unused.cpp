#include "bucket_sorting.hpp"
#include <algorithm>
#include <list>

using namespace std;
const static size_t safe_buf_len = comp_len_4 / comp_len_1 + 1;

uint32_t Buckets::_r_extend(uint32_t target, uint32_t amount) {
  MY_ASSERT(amount);
  uint32_t prev_idx = prev_contiguous(target);
  if (prev_idx == bucket_len)
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
  MY_ASSERT(from != to);
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
      MY_ASSERT(!overflow_count(ok));
      from_buffer.second = 0;
    }
    if(to_buffer.second && (!from_has_data || to_buffer.second <= uint32_t(from_it - from_wr))) {
      auto ok = from_wr.write_and_advance(to_buffer.first);
      MY_ASSERT(!overflow_count(ok));
      to_buffer.second = 0;
    }
  }
  while(from_buffer.second || to_buffer.second || from_has_data || to_has_data);

  MY_ASSERT((to_wr - begin(to)) + (from_wr - begin(from)) == int32_t(lens[from] + lens[to]));
  update(from, from_wr);
  update(to, to_wr);
  return 0;
}

uint32_t Buckets::_add_number(decimal_t decimal) {
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

    MY_ASSERT(delta < sum && next <= sum && extra_len <= (int32_t)(2*comp_len_4));
    list<uint32_t> buffer = { delta, next };

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

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

uint32_t _write_compressed(uint32_t number, uint8_t* start, uint8_t bit_offset, uint8_t* end) {
  MY_ASSERT(bit_offset < 8);
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

size_t _compress_len(comp_int_t comp) {
  if(!(comp[0] & 0x40))
    return comp_len_1;
  if(!(comp[1] & 0x40))
    return comp_len_2;
  if(!(comp[2] & 0x80))
    return comp_len_3;
  return comp_len_4;
}

int_len_t _decompress(uint8_t* start, uint8_t bit_offset) {
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

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

comp_int_t _zigzag_compress(uint32_t number) {
  MY_ASSERT(number < comp_max_4);
  comp_int_t comp;
  int32_t biased = number - bias;
  uint32_t zigzag = (biased >> 31) ^ (biased << 1);

  if(zigzag < comp_max_1) {
    comp[0] = (zigzag & 0x3f);
  }
  else if(zigzag < comp_max_2) {
    comp[0] = (zigzag & 0x3f) | 0x40 | ((zigzag << 1) & 0x80);
    comp[1] = ((zigzag >> 7) & 0x3f);
    MY_ASSERT(comp[0] > 0);
  }
  else if(zigzag < comp_max_3) {
    comp[0] = (zigzag & 0x3f) | 0x40 | ((zigzag << 1) & 0x80);
    comp[1] = ((zigzag >> 7) & 0x3f) | 0x40 | ((zigzag >> 6) & 0x80);
    comp[2] = (zigzag >> 14) & 0x7f;
    MY_ASSERT(comp[0] > 0 && comp[1] > 0);
  }
  else {
    comp[0] = (zigzag & 0x3f) | 0x40 | ((zigzag << 1) & 0x80);
    comp[1] = ((zigzag >> 7) & 0x3f) | 0x40 | ((zigzag >> 6) & 0x80);
    comp[2] = ((zigzag >> 14) & 0x7f) | 0x80;
    comp[3] = (zigzag >> 21) & 0xff;
    MY_ASSERT(comp[0] > 0 && comp[1] > 0 && comp[2] > 0);
  }
  return comp;
}

size_t _zigzag_compress_len(uint32_t number) {
  const int32_t lower_bound1 = comp_max_1/2;
  const int32_t lower_bound2 = comp_max_2/2;
  const int32_t lower_bound3 = comp_max_3/2;
  int32_t biased = number - bias;
  if(biased >= -lower_bound1 && biased < lower_bound1)
    return comp_len_1;
  if(biased >= -lower_bound2 && biased < lower_bound2)
    return comp_len_2;
  if(biased >= -lower_bound3 && biased < lower_bound3)
    return comp_len_3;
  return comp_len_4;
}

uint32_t _zigzag_decompress(comp_int_t comp) {
  if((comp[0] & 0x40) == 0) {
    int32_t zigzag = comp[0] & 0x3f;
    MY_ASSERT((uint32_t)zigzag < comp_max_1);
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
  if((comp[1] & 0x40) == 0) {
    int32_t zigzag = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    zigzag         |= ((comp[1] >> 1) & 0x1f) << 8;
    MY_ASSERT((uint32_t)zigzag < comp_max_2);
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
  if((comp[2] & 0x80) == 0) {
    int32_t zigzag = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    zigzag         |= ( ((comp[1] >> 1) & 0x1f) | ((comp[1] >> 2) & 0x20) | ((comp[2] << 6) & 0xc0) ) << 8;
    zigzag         |= ( ((comp[2] >> 2) & 0x1f) ) << 16;
    MY_ASSERT((uint32_t)zigzag < comp_max_3);
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
  else {
    MY_ASSERT(comp[3] > 0);
    int32_t zigzag = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    zigzag         |= ( ((comp[1] >> 1) & 0x1f) | ((comp[1] >> 2) & 0x20) | ((comp[2] << 6) & 0xc0) ) << 8;
    zigzag         |= ( ((comp[2] >> 2) & 0x1f) | ((comp[3] << 5) & 0xe0) ) << 16;
    zigzag         |= ( ((comp[3] >> 3) & 0x1f) ) << 24;
    zigzag = (static_cast<uint32_t>(zigzag) >> 1u) ^ -(zigzag & 1);
    return zigzag + bias;
  }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

comp_int_t _unbiased_compress(uint32_t number) {
  MY_ASSERT(number < comp_max_4);
  comp_int_t comp;

  if(number < comp_max_1) {
    comp[0] = (number & 0x3f);
  }
  else if(number < comp_max_2) {
    comp[0] = (number & 0x3f) | 0x40 | ((number << 1) & 0x80);
    comp[1] = ((number >> 7) & 0x3f);
    MY_ASSERT(comp[0] > 0);
  }
  else if(number < comp_max_3) {
    comp[0] = (number & 0x3f) | 0x40 | ((number << 1) & 0x80);
    comp[1] = ((number >> 7) & 0x3f) | 0x40 | ((number >> 6) & 0x80);
    comp[2] = (number >> 14) & 0x7f;
    MY_ASSERT(comp[0] > 0 && comp[1] > 0);
  }
  else {
    comp[0] = (number & 0x3f) | 0x40 | ((number << 1) & 0x80);
    comp[1] = ((number >> 7) & 0x3f) | 0x40 | ((number >> 6) & 0x80);
    comp[2] = ((number >> 14) & 0x7f) | 0x80;
    comp[3] = (number >> 21) & 0xff;
    MY_ASSERT(comp[0] > 0 && comp[1] > 0 && comp[2] > 0);
  }
  return comp;
}

size_t _unbiased_compress_len(uint32_t number) {
  if(number < comp_max_1)
    return comp_len_1;
  if(number < comp_max_2)
    return comp_len_2;
  if(number < comp_max_3)
    return comp_len_3;
  return comp_len_4;
}

uint32_t _unbiased_decompress(comp_int_t comp) {
  if((comp[0] & 0x40) == 0) {
    int32_t number = comp[0] & 0x3f;
    MY_ASSERT((uint32_t)number < comp_max_1);
    return number;
  }
  if((comp[1] & 0x40) == 0) {
    int32_t number = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    number         |= ((comp[1] >> 1) & 0x1f) << 8;
    MY_ASSERT((uint32_t)number < comp_max_2);
    return number;
  }
  if((comp[2] & 0x80) == 0) {
    int32_t number = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    number         |= ( ((comp[1] >> 1) & 0x1f) | ((comp[1] >> 2) & 0x20) | ((comp[2] << 6) & 0xc0) ) << 8;
    number         |= ( ((comp[2] >> 2) & 0x1f) ) << 16;
    MY_ASSERT((uint32_t)number < comp_max_3);
    return number;
  }
  else {
    MY_ASSERT(comp[3] > 0);
    int32_t number = (comp[0] & 0x3f) | ((comp[0] >> 1) & 0x40) | ((comp[1] << 7) & 0x80) ;
    number         |= ( ((comp[1] >> 1) & 0x1f) | ((comp[1] >> 2) & 0x20) | ((comp[2] << 6) & 0xc0) ) << 8;
    number         |= ( ((comp[2] >> 2) & 0x1f) | ((comp[3] << 5) & 0xe0) ) << 16;
    number         |= ( ((comp[3] >> 3) & 0x1f) ) << 24;
    return number;
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
void _test_compress_len() {
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

void test_bucket_swap_same_len() {
  auto buckets = build_and_fill_first_bucket();

  DECLARE_INPUT;
  auto shuf_input = input;
  random_shuffle(shuf_input.begin(), shuf_input.end());
  fill_bucket(buckets, 3, shuf_input);

  auto ori_0_end = buckets.end(0);
  auto ori_3_end = buckets.end(3);

  buckets.swap(0, 3);
  MY_ASSERT(ori_0_end == buckets.end(0) && ori_3_end == buckets.end(3));
  compare_bucket_to_ref(shuf_input, buckets.at(0));
  compare_bucket_to_ref(input, buckets.at(3));

  buckets.swap(0, 3);
  MY_ASSERT(ori_0_end == buckets.end(0) && ori_3_end == buckets.end(3));
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
    MY_ASSERT(ori_0_len == buckets.lens[1] && ori_1_len == buckets.lens[0]);

    buckets.swap(1, 0);
    compare_bucket_to_ref(input1, buckets.at(0));
    compare_bucket_to_ref(input2, buckets.at(1));
    MY_ASSERT(ori_0_len == buckets.lens[0] && ori_1_len == buckets.lens[1]);
  }
}

void test_bucket_swap_one_empty() {
  DECLARE_INPUT;
  auto buckets = build_and_fill_first_bucket();

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(1));
  MY_ASSERT(buckets.begin(0) == buckets.end(0));

  buckets.swap(0, 1);
  compare_bucket_to_ref(input, buckets.at(0));
  MY_ASSERT(buckets.begin(1) == buckets.end(1));
}
*/

