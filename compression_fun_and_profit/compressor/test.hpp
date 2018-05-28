#pragma once
#include <bit_codec.hpp>
#include <byte_codec.hpp>
#include <common.hpp>
#include <series_io.hpp>

#include <algorithm>

///////////////////////////////////////////////////////////////////////////////////////////////////

void test_read_prob_dstrb_from_file();
void test_read_series_delta_from_file();
inline void test_series_io() {
  LOG("--- start suite ---");
  test_read_prob_dstrb_from_file();
  test_read_series_delta_from_file();
}

///////////////////////////////////////////////////////////////////////////////////////////////////

void test_number_byte_codec();
void test_synthetic_series_byte_codec();
void test_real_series_byte_codec();
inline void test_byte_codec() {
  LOG("--- start suite ---");
  test_number_byte_codec();
  test_synthetic_series_byte_codec();
  test_real_series_byte_codec();
}

///////////////////////////////////////////////////////////////////////////////////////////////////

void test_decompose_and_collapse_tails();
void test_bit_encoder_write_coef_gaussian();
void test_bit_encoder_write_coef_ordered();
void test_synthetic_series_bit_codec();
void test_real_series_bit_codec();
void test_bit_codec_coef_from(const char* dstrb_name);
inline void test_bit_codec() {
  LOG("--- start suite ---");
  test_decompose_and_collapse_tails();
  test_bit_encoder_write_coef_gaussian();
  test_bit_encoder_write_coef_ordered();
  test_bit_codec_coef_from("ordered_series.prob");
  test_bit_codec_coef_from("gaussian_series_mu_0.prob");
  test_bit_codec_coef_from("gaussian_series_mu_666.prob");
  test_synthetic_series_bit_codec();
  test_real_series_bit_codec();
}

///////////////////////////////////////////////////////////////////////////////////////////////////

inline void test_all() {
  LOG("### start all ###");
  //test_series_io();
  //test_byte_codec();
  test_bit_codec();
  LOG("### end all ###");
}

///////////////////////////////////////////////////////////////////////////////////////////////////

bool operator ==(const SeriesMetadata&lhs, const SeriesMetadata&rhs);
bool is_in_dstrb(const ProbDstrb_t& dstrb, symb_t symb);
ProbDstrb_t sample_from_prob_dstrb(const ProbDstrb_t& dstrb, const std::vector<symb_t>& symbs);

template<class S>
bool compare_series(const S& lhs, const S& rhs) {
  auto meta_ok = std::equal(RANGE(lhs.meta), RANGE(rhs.meta));
  auto data_ok = std::equal(RANGE(lhs.data), RANGE(rhs.data));
  if(!data_ok)
    for(uint32_t i=0; i<std::min(lhs.count(), rhs.count()); ++i)
    for(uint32_t j=0; i<std::min(lhs.data[i].size(), rhs.data[i].size()); ++j)
      TEST_ASSERT(lhs.data[i][j] == rhs.data[i][j]);
  return meta_ok && data_ok && lhs.dformat == rhs.dformat;
}

template<class S>
std::unique_ptr<S> negate_series(const S&input) {
  auto series = std::make_unique<S>(input);
  for(auto& data : series->data)
    std::transform(RANGE(data), data.begin(), [](auto i) { return -i; });
  return series;
}

