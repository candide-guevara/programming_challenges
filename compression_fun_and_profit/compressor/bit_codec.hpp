#pragma once
#include <vector>

#include <common.hpp>
#include <series_io.hpp>

struct BitStream {
  std::vector<uint8_t> buffer;
  size_t write_pos, read_pos;

  void write_bit(uint8_t bit);
  uint8_t read_bit();
};

struct Interval {
  BitStream buffer;
  ProbDstrb prob;
  prob_t min, max;

  void write_number(delta_t number);
  void write_end_marker();
  void write_coef(symb_t coef, size_t times);
  void lerp_by_prob(symb_t coef);
  void expand_and_write_simple();
  void expand_hard();
};

std::unique_ptr<Compressed> bit_compress_from_delta(const Series& input, const ProbDstrb& prob);
std::unique_ptr<Series> delta_from_bit_compress(const Compressed& input, const ProbDstrb& prob);

