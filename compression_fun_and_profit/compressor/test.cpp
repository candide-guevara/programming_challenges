#include <test.hpp>

///////////////////////////////////////////////////////////////////////////////////////////////////

void test_read_prob_dstrb_from_file() {
  LOG("start");
  auto dstrb = read_prob_dstrb_from_file("prob_dstrb_bics_1_tech.txt");
  TEST_ASSERT(dstrb->size() >= ALPHA_LEN * 2); 
  TEST_ASSERT(std::is_sorted(RANGE(*dstrb), [](auto a, auto b) { return cumsum(a) < cumsum(b); }));
  TEST_ASSERT(std::all_of(RANGE(*dstrb), [](auto t) { return weight(t) < MAX_PROB; }));
  TEST_ASSERT(cumsum(dstrb->back()) == MAX_PROB);
  //LOG("\n" << prob_to_string(*dstrb));
}

void test_read_series_delta_from_file() {
  LOG("start");
  auto series = read_series_from_file("ordered_series.bin");
  TEST_ASSERT(series->count() == 3);
  TEST_ASSERT(std::all_of(RANGE(series->meta), [](auto& m) { return m.count == 2000; }));
  TEST_ASSERT(std::all_of(RANGE(series->data), [](auto& d) { return std::is_sorted(RANGE(d)); }));
  //LOG("\n" << series_to_string(*series));
}

///////////////////////////////////////////////////////////////////////////////////////////////////

void test_number_byte_codec() {
  LOG("start");
  std::vector<delta_t> small_tests = {0, 1, 2, 3, ALPHA_LEN-1, ALPHA_LEN, ALPHA_LEN+1, 2*ALPHA_LEN, byte_codec_lvl1()-1, byte_codec_lvl1() }; 
  std::vector<delta_t> medium_tests = { byte_codec_lvl1()+1, byte_codec_lvl1()+ALPHA_LEN, byte_codec_lvl2()-1, byte_codec_lvl2() }; 
  std::vector<delta_t> large_tests = { byte_codec_lvl2()+1, byte_codec_lvl2()+ALPHA_LEN, byte_codec_lvl2()+byte_codec_lvl1() }; 

  auto driver = [](auto data, auto encode_func, auto decode_func) {
    uint8_t buffer[5];
    for(auto num : data) {
      encode_func(num, buffer);
      auto decoded = decode_func(buffer);
      TEST_ASSERT(num == decoded);
      encode_func(-num, buffer);
      decoded = decode_func(buffer);
      TEST_ASSERT(-num == decoded);
    }
  };

  driver(small_tests, byte_encode_small, byte_decode_small);
  driver(medium_tests, byte_encode_medium, byte_decode_medium);
  driver(large_tests, byte_encode_large, byte_decode_large);
}

void test_synthetic_series_byte_codec() {
  LOG("start");
  std::unique_ptr<Series> input[6] = {};
  input[0] = read_series_from_file("ordered_series.bin");
  input[1] = negate_series(*input[0]);
  input[2] = read_series_from_file("gaussian_series_mu_0.bin");
  input[3] = negate_series(*input[2]);
  input[4] = read_series_from_file("gaussian_series_mu_666.bin");
  input[5] = negate_series(*input[4]);
  for(const auto& series : input) {
    auto compressed = byte_compress_from_delta(*series);
    auto series2 = delta_from_byte_compress(*compressed);
    TEST_ASSERT(compare_series(*series, *series2));
  }
}

void test_real_series_byte_codec() {
  LOG("start");
  std::unique_ptr<Series> input[1] = {};
  input[0] = read_series_from_file("secout_bics_1_tech.bin");
  input[1] = read_series_from_file("secout_xchng_us.bin");
  for(const auto& series : input) {
    auto compressed = byte_compress_from_delta(*series);
    auto series2 = delta_from_byte_compress(*compressed);
    TEST_ASSERT(compare_series(*series, *series2));
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

void test_synthetic_series_bit_codec() {
  LOG("start");
  /*std::unique_ptr<Series> input[6] = {};
  input[0] = read_series_from_file("ordered_series.bin");
  input[1] = negate_series(*input[0]);
  input[2] = read_series_from_file("gaussian_series_mu_0.bin");
  input[3] = negate_series(*input[2]);
  input[4] = read_series_from_file("gaussian_series_mu_666.bin");
  input[5] = negate_series(*input[4]);
  for(const auto& series : input) {
    auto compressed = bit_compress_from_delta(*series);
    auto series2 = delta_from_bit_compress(*compressed);
    TEST_ASSERT(compare_series(*series, *series2));
  }*/
}

void test_real_series_bit_codec() {
  LOG("start");
  /*std::unique_ptr<Series> input[1] = {};
  input[0] = read_series_from_file("secout_bics_1_tech.bin");
  input[1] = read_series_from_file("secout_xchng_us.bin");
  for(const auto& series : input) {
    auto compressed = bit_compress_from_delta(*series);
    auto series2 = delta_from_bit_compress(*compressed);
    TEST_ASSERT(compare_series(*series, *series2));
  }*/
}

///////////////////////////////////////////////////////////////////////////////////////////////////

bool operator ==(const SeriesMetadata&lhs, const SeriesMetadata&rhs) {
  return lhs.sid == rhs.sid 
          && lhs.min == rhs.min && lhs.max == rhs.max
          && lhs.count == rhs.count && lhs.start == rhs.start;
}

