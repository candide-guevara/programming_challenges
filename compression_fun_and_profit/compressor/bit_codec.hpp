#pragma once
#include <unordered_map>

#include <common.hpp>
#include <series_io.hpp>

using SymbTable_t = std::unordered_map<symb_t, std::pair<prob_t,size_t>>;
using SymbRank_t = std::unordered_map<symb_t, size_t>;
using CumSum_t = std::vector<prob_t>;
using IdxSymb_t = std::vector<symb_t>;

struct BitWriter {
  static constexpr size_t INIT_LEN = 365 * 5 * 2;
  std::vector<uint8_t> buffer;
  size_t write_pos;

  BitWriter();
  void write_bit(uint8_t bit);
};

struct BitReader {
  const uint8_t* buffer;
  size_t read_pos;

  void load_data(const uint8_t *start);
  uint8_t read_bit();
};

struct BitEncoder {
  BitWriter buffer;
  SymbTable_t symb_tab;

  BitEncoder(const ProbDstrb_t& _prob);
  void build_symbol_table(const ProbDstrb_t& prob);
  void move_comp_data(std::vector<uint8_t>& dest);
  SymbTable_t::mapped_type encoding_for_range(prob_t min, prob_t max);  

  void write_number(delta_t number);
  void write_end_marker();
  void write_coef(symb_t coef, size_t times);
};

struct BitDecoder {
  BitReader buffer;
  CumSum_t cumsum;
  IdxSymb_t idx_to_symb;
  SymbRank_t symb_to_rank;
  delta_t carry;

  BitDecoder(const ProbDstrb_t& _prob);
  void build_cumsum(const ProbDstrb_t& prob);
  void build_idx_to_symb(const ProbDstrb_t& prob);
  void build_symb_to_rank(const ProbDstrb_t& prob);

  void load_data(const uint8_t* start);
  delta_t read_number();
  symb_t read_symbol();
};

std::array<int32_t, PROB_BASE_LEN+1> decompose_and_collapse_tails(delta_t number);
std::unique_ptr<Compressed> bit_compress_from_delta(const Series& input, const ProbDstrb_t& prob);
std::unique_ptr<Series> delta_from_bit_compress(const Compressed& input, const ProbDstrb_t& prob);

