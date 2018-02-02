#include "integer_sorting.hpp"

buffer_t buid_buffer() {
  buffer_t buffer = { new uint8_t[2 * buffer_len] };
  buffer.second = 0;
  buffer.oracle = std::make_pair(0, buffer.first);
  return buffer;
}

const uint32_t lvl2 = 252;
len_comp_t encode(uint32_t number) {
  const uint32_t slots = 0xff - lvl2;
  const uint32_t max_lvl2 = slots * 0x100 + lvl2 - 1;
  len_comp_t result = {1, {{(uint8_t)number}}};
  compress_t& comp = result.second;

  if(number >= lvl2 && number < max_lvl2) {
    result.first = 2;
    comp[0] = (number - lvl2 + 1) / 0x100 + lvl2;
    comp[1] = (number - lvl2 + 1) % 0x100;
  }
  else if(number >= max_lvl2) {
    result.first = max_len;
    comp[0] = 255;
    comp[1] = (number & 0xff);
    comp[2] = ((number >> 8) & 0xff);
    comp[3] = ((number >> 16) & 0xff);
    comp[4] = ((number >> 24) & 0xff);
  }
  return result;
}

num_shift_t decode(const uint8_t *start) {
  num_shift_t result = {start[0],1};
  if(start[0] >= lvl2 && start[0] < 0xff) {
    result.first = (start[0] - lvl2) * 0x100 + start[1] + lvl2 - 1;
    result.second = 2;
  }
  if(start[0] == 0xff) {
    result.first = start[1] + (start[2] << 8) + (start[3] << 16) + (start[4] << 24);
    result.second = max_len;
  }
  return result;
}

void update_oracle(buffer_t& buffer, uint32_t number, uint8_t* position) {
  buffer.oracle = std::make_pair(number, position);
}

num_pos_t oracle_guess_start(buffer_t& buffer, uint32_t number) {
  auto result = std::make_pair(0, buffer.first);
  if(number > buffer.oracle.first)
    result = buffer.oracle;
  return result;
}

void add_to_buffer(buffer_t& buffer, uint32_t number) {
  auto end_bound = buffer.first + buffer.second;
  auto num_pos = oracle_guess_start(buffer, number);
  uint32_t sum = num_pos.first;
  uint8_t* i = num_pos.second;

  while(i<end_bound) {
    auto num_shift = decode(i);
    sum += num_shift.first;
    if(number < sum) {
      uint32_t delta = number - (sum - num_shift.first);
      uint32_t next = sum - number;
      add_raw_and_shift(buffer, i, i+num_shift.second, delta, next);
      update_oracle(buffer, number-delta, i);
      return;
    }
    i += num_shift.second;
  }

  uint32_t delta = number - sum;
  auto len_comp = encode(delta);
  uint8_t* start = len_comp.second.data();
  std::copy(start, start+len_comp.first, buffer.first + buffer.second);
  buffer.second += len_comp.first;
}

void add_raw_and_shift(buffer_t& buffer, uint8_t* start, uint8_t* move, uint32_t first, uint32_t second) {
  auto len_comp_1 = encode(first);
  auto len_comp_2 = encode(second);
  uint8_t* end = buffer.first + buffer.second;
  int32_t extra_len = len_comp_1.first + len_comp_2.first - (move - start);

  if(extra_len > 0)
    std::copy_backward(move, end, end+extra_len);
  if(extra_len < 0)
    std::copy(move, end, move+extra_len);

  uint8_t* comp_start = len_comp_1.second.data();
  start = std::copy(comp_start, comp_start+len_comp_1.first, start);
  comp_start = len_comp_2.second.data();
  std::copy(comp_start, comp_start+len_comp_2.first, start);

  buffer.second += extra_len;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

uint32_t my_format(uint8_t n) {
  return (uint32_t)n;
}

void* my_format(uint8_t* p) {
  return (void*)p;
}

template<class T>
T my_format(T input) {
  return input;
}

template<class T, class V>
std::string my_format(const std::pair<T,V>& input) {
  std::stringstream ss;
  ss << "(" << my_format(input.first) << "," << my_format(input.second) << ")";
  return ss.str();
}

template<class V>
std::string print_collection(const V& input) {
  std::stringstream ss;
  ss << "[";
  for(auto i : input) ss << my_format(i) << ",";
  ss << "]";
  return ss.str();
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::vector<uint32_t> generate_rand_uint_input(uint32_t len, uint32_t max_item_val) {
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dist(0, max_item_val);

  std::vector<uint32_t> input;
  input.reserve(len);
  for(uint32_t i=0; i<len; ++i)
    input.push_back(dist(gen));
  return input;
}

buffer_t sort_one_million_in_one_mb(const std::vector<uint32_t>& input) {
  buffer_t buffer = buid_buffer();
  uint32_t progress = 0;
  for(auto i : input) {
    if(++progress % (input_len/100) == 0) {
      LOG("Adding to buffer : len_kb=" << (buffer.second/1024) << " progress=" << progress);
      //LOG("oracle : " << print_collection(buffer.oracle));
    }
    add_to_buffer(buffer, i);
  }
  return buffer;
}

uint32_t validate(const buffer_t& buffer, const std::vector<uint32_t>& ref) {
  uint32_t value = 0;
  uint32_t error_count = 0;
  const uint8_t* cursor = buffer.first;
  std::vector<uint32_t> sorted_ref = ref;
  std::sort(sorted_ref.begin(), sorted_ref.end());
  histo_t histo;

  for(auto i : sorted_ref) {
    auto num_shift = decode(cursor);
    cursor += num_shift.second;
    value += num_shift.first;
    if(num_shift.first < 256) ++histo[num_shift.first];
    else ++histo[num_shift.first/256 + 1000];
    if(i != value)
      error_count += 1;
  }
  LOG("frequencies : " << print_collection(histo));
  return error_count;
}

int main(void) {
  auto input = generate_rand_uint_input(input_len, max_input);
  buffer_t sorted_buf = sort_one_million_in_one_mb(input);
  uint32_t error_count = validate(sorted_buf, input);
  LOG("final buffer size_kb=" << (sorted_buf.second/1024) << " errors=" << error_count);
  return 0;
}
