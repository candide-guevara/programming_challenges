#pragma once
#include <vector>

#include <common.hpp>
#include <series_io.hpp>

using ScaledProb = std::vector<prob_t>;

struct BitStream {
  static constexpr size_t INIT_LEN = 16 * 1024;
  std::vector<uint8_t> buffer;
  size_t write_pos, read_pos;

  void write_bit(uint8_t bit);
  prob_t read_bit();
};

struct Interval {
  BitStream buffer;
  const ProbDstrb& prob;
  prob_t min, max;

  Interval(const ProbDstrb& _prob);

  void write_number(delta_t number);
  void write_end_marker();
  void write_coef(symb_t coef, size_t times);
  void lerp_by_prob(symb_t coef);
  void expand_and_write_simple();
  void expand_hard();
  void move_comp_data(std::vector<uint8_t>& dest);

  delta_t read_number();
  symb_t read_symbol();
  ScaledProb lerp_prob_dstrb();
};

std::unique_ptr<Compressed> bit_compress_from_delta(const Series& input, const ProbDstrb& prob);
std::unique_ptr<Series> delta_from_bit_compress(const Compressed& input, const ProbDstrb& prob);

