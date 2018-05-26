#pragma once
#include <array>

#include <series_io.hpp>

constexpr uint8_t PREFIX_SMALL = 0;
constexpr uint8_t PREFIX_SMALL_LEN = 1;
constexpr uint8_t PREFIX_MEDIUM = 1;
constexpr uint8_t PREFIX_MEDIUM_LEN = 2;
constexpr uint8_t PREFIX_LARGE = 3;
constexpr uint8_t PREFIX_LARGE_LEN = 2;
constexpr size_t  DECOMP_BASE_LEN = 5;

constexpr delta_t byte_codec_lvl1() {
  constexpr size_t A = ALPHA_LEN;
  return (A-1) + A*(A-1) + A*A;
}
constexpr delta_t byte_codec_lvl2() {
  constexpr size_t A = ALPHA_LEN;
  return (A-1) + A*(A-1) + A*A*(A-1) + A*A*A*(A-1) + A*A*A*A*3;
}

void byte_compress_numbers(const std::vector<delta_t>& input, std::vector<uint8_t>& output);
void byte_decompress_numbers(const std::vector<uint8_t>& input, std::vector<delta_t>& output);

void byte_encode_small(delta_t number, uint8_t* buffer);
void byte_encode_medium(delta_t number, uint8_t* buffer);
void byte_encode_large(delta_t number, uint8_t* buffer);

delta_t byte_decode_small(const uint8_t* buffer);
delta_t byte_decode_medium(const uint8_t* buffer);
delta_t byte_decode_large(const uint8_t* buffer);

std::unique_ptr<Compressed> byte_compress_from_delta(const Series& input);
std::unique_ptr<Series> delta_from_byte_compress(const Compressed& input);

