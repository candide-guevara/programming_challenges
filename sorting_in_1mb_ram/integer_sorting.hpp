#pragma once
#include <algorithm>
#include <array>
#include <iostream>
#include <map>
#include <random>
#include <sstream>
#include <utility>
#include <vector>

#pragma GCC diagnostic ignored "-Wunused-function"
#define LOG(msg) std::cout << msg << std::endl;

const uint8_t max_len = 6;
const uint32_t input_mask = 100000000;
const uint32_t input_len  = 1000000;
const uint32_t buffer_len = 1024*1024;
const uint32_t max_input = input_mask - 1;
using compress_t = std::array<uint8_t, max_len>;
using len_comp_t = std::pair<uint8_t, compress_t>;
using num_shift_t = std::pair<uint32_t, uint8_t>;
using num_pos_t = std::pair<uint32_t, uint8_t*>;
using histo_t = std::map<uint32_t, uint32_t>;

struct buffer_t {
  uint8_t* first;
  uint32_t second;
  num_pos_t oracle;
};

buffer_t buid_buffer();
len_comp_t encode(uint32_t number);
num_shift_t decode(const uint8_t *start);
void update_oracle(buffer_t& buffer, uint32_t number, uint8_t* position);
num_pos_t oracle_guess_start(buffer_t& buffer, uint32_t number);
void add_to_buffer(buffer_t& buffer, uint32_t number);
void add_raw_and_shift(buffer_t& buffer, uint8_t* start, uint8_t* move, uint32_t first, uint32_t second);

std::vector<uint32_t> generate_rand_uint_input(uint32_t len, uint32_t max_item_val);
uint32_t validate(const buffer_t& buffer, const std::vector<uint32_t>& ref);
buffer_t sort_one_million_in_one_mb(const std::vector<uint32_t>& input);
