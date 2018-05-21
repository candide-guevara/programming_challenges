#include <series_io.hpp>
#include <byte_codec.hpp>

#include <algorithm>
#include <functional>

void test_read_prob_dstrb_from_file() {
  auto dstrb = read_prob_dstrb_from_file("prob_dstrb_bics_1_tech.txt");
  TEST_ASSERT(dstrb->size() >= ALPHA_LEN * 2); 
  TEST_ASSERT(std::is_sorted(RANGE(*dstrb), [](auto a, auto b) { return cumsum(a) < cumsum(b); }));
  TEST_ASSERT(std::all_of(RANGE(*dstrb), [](auto t) { return weight(t) < MAX_PROB; }));
  TEST_ASSERT(cumsum(dstrb->back()) == MAX_PROB);
  //LOG("\n" << prob_to_string(*dstrb));
}

void test_read_series_delta_from_file() {
  auto series = read_series_from_file("ordered_series.bin");
  TEST_ASSERT(series->count() == 3);
  TEST_ASSERT(std::all_of(RANGE(series->meta), [](auto& m) { return m.count == 2000; }));
  TEST_ASSERT(std::all_of(RANGE(series->data), [](auto& d) { return std::is_sorted(RANGE(d)); }));
  //LOG("\n" << series_to_string(*series));
}

void test_number_byte_codec() {
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

bool operator ==(const SeriesMetadata&lhs, const SeriesMetadata&rhs) {
  return lhs.sid == rhs.sid 
          && lhs.min == rhs.min && lhs.max == rhs.max
          && lhs.count == rhs.count && lhs.start == rhs.start;
}

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

void test_series_byte_codec() {
  //std::unique_ptr<Series> input[6] = {};
  //input[0] = read_series_from_file("ordered_series.bin");
  //input[1] = negate_series(*input[0]);
  //input[2] = read_series_from_file("gaussian_series_mu_0.bin");
  //input[3] = negate_series(*input[2]);
  //input[4] = read_series_from_file("gaussian_series_mu_666.bin");
  //input[5] = negate_series(*input[4]);
  std::unique_ptr<Series> input[2] = {};
  input[0] = read_series_from_file("gaussian_series_mu_666.bin");
  input[1] = negate_series(*input[0]);
  for(const auto& series : input) {
    auto compressed = byte_compress_from_delta(*series);
    auto series2 = delta_from_byte_compress(*compressed);
    TEST_ASSERT(compare_series(*series, *series2));
  }
}

void byte_compress_real_series() {
  std::string names[] = {"secout_bics_1_tech.bin", "secout_xchng_us.bin"};
  for(auto& name : names) {
    auto series = read_series_from_file(name);
    auto compressed = byte_compress_from_delta(*series);
    auto data_ratio = 1.0*series->data_bytes() / compressed->data_bytes();
    auto total_ratio = 1.0*series->total_bytes() / compressed->total_bytes();
    LOG(name << " : data_ratio=" << data_ratio << ", total_ratio=" << total_ratio
        << ", data=" << series->data_bytes() << ", total=" << series->total_bytes() );
  }
}

int main(int argc, char** argv) {
  //test_read_prob_dstrb_from_file();
  //test_read_series_delta_from_file();
  //test_number_byte_codec();
  //test_series_byte_codec();
  byte_compress_real_series();
  return 0;
}

