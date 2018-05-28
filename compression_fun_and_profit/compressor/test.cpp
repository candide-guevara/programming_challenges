#include <test.hpp>

///////////////////////////////////////////////////////////////////////////////////////////////////

void test_read_prob_dstrb_from_file() {
  LOG("start");
  auto dstrb = read_prob_dstrb_from_file("secout_bics_1_tech.prob");
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

void test_decompose_and_collapse_tails() {
  LOG("start");
  std::vector<delta_t> inputs = { 0, 1, 7, ALPHA_LEN, 1+2*ALPHA_LEN, A_POW(2), 1+2*ALPHA_LEN+11*A_POW(2),
                                  MAX_SYMB, MAX_SYMB+A_POW(5), 1+ALPHA_LEN+A_POW(2)+A_POW(3)+A_POW(4) };
  std::vector<Decomp_t> expects = { {0}, {1,0}, {7,0}, {0,1,0}, {1,2,0}, {0,0,1,0}, {1,2,11,0},
                                    {0,0,0,0,1}, {0,0,0,0,ALPHA_LEN+1}, {1,1,1,1,1} };
  for(uint32_t i=0; i<inputs.size(); ++i) {
    auto expect = expects[i];
    auto input = inputs[i];
    auto decomp = decompose_and_collapse_tails(input);
    TEST_ASSERT(decomp == expect);

    std::transform(RANGE(expects[i]), expect.begin(), [](auto d) { return -d; });
    decomp = decompose_and_collapse_tails(-input);
    TEST_ASSERT(decomp == expect);
  }
}

void test_bit_encoder_write_coef_gaussian() {
  LOG("start");
  auto dstrb = read_prob_dstrb_from_file("gaussian_series_mu_0.prob");
  std::vector<symb_t> inputs = { 0, 1, 7, ALPHA_LEN, 2*ALPHA_LEN };
  BitEncoder encoder(*dstrb);

  for(auto symb : inputs) {
    encoder.write_coef(symb, 1);
    encoder.buffer.skip_to_next_byte();
    encoder.write_coef(-symb, 1);
    encoder.buffer.skip_to_next_byte();
  }
  encoder.write_end_marker();
  TEST_ASSERT(encoder.buffer.buffer.size() > inputs.size());

  auto subpop = sample_from_prob_dstrb(*dstrb, inputs);
  //LOG(encoder.buffer.buffer_to_str());
  //LOG(prob_to_string(subpop, true));
}

void test_bit_encoder_write_coef_ordered() {
  LOG("start");
  auto dstrb = read_prob_dstrb_from_file("ordered_series.prob");
  std::vector<symb_t> inputs = { 1, 7, ALPHA_LEN, 2*ALPHA_LEN, 3*ALPHA_LEN };
  BitEncoder encoder(*dstrb);

  for(auto symb : inputs) {
    encoder.write_coef(symb, 1);
    encoder.buffer.skip_to_next_byte();
  }
  encoder.write_end_marker();
  TEST_ASSERT(encoder.buffer.buffer.size() > inputs.size());

  auto subpop = sample_from_prob_dstrb(*dstrb, inputs);
  //LOG(encoder.buffer.buffer_to_str());
  //LOG(prob_to_string(subpop, true));
}

void test_bit_codec_coef_from(const char* dstrb_name) {
  LOG("start : " << dstrb_name);
  auto dstrb = read_prob_dstrb_from_file(dstrb_name);
  BitEncoder encoder(*dstrb);
  BitDecoder decoder(*dstrb);

  //std::vector<symb_t> inputs = { 1, 7, ALPHA_LEN, 2*ALPHA_LEN, 3*ALPHA_LEN };
  std::vector<symb_t> inputs;
  for(symb_t symb = 0; symb < (symb_t)ALPHA_LEN; ++symb) {
    if(is_in_dstrb(*dstrb, symb)) inputs.push_back(symb);
    if(is_in_dstrb(*dstrb, symb*ALPHA_LEN)) inputs.push_back(symb*ALPHA_LEN);
    if(is_in_dstrb(*dstrb, -symb)) inputs.push_back(-symb);
    if(is_in_dstrb(*dstrb, -symb*ALPHA_LEN)) inputs.push_back(-symb*ALPHA_LEN);
  }

  // the first write will prime the carry
  encoder.write_coef(inputs[0], 1);
  for(auto symb : inputs)
    encoder.write_coef(symb, 1);
  encoder.write_end_marker();

  auto& encoded = encoder.buffer.buffer;
  decoder.load_data_and_prime_carry(encoded.data());
  TEST_ASSERT(decoder.carry == inputs[0]);

  for(auto exp_symb : inputs) {
    auto dec_symb = decoder.read_symbol();
    LOG("exp_symb=" << exp_symb << " / " << "dec_symb=" << dec_symb);
    TEST_ASSERT(exp_symb == dec_symb);
  }
}

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

ProbDstrb_t sample_from_prob_dstrb(const ProbDstrb_t& dstrb, const std::vector<symb_t>& symbs) {
  ProbDstrb_t sample(symbs.size(), ProbTuple{});
  std::transform(RANGE(symbs), sample.begin(), [&dstrb](auto symb) {
    auto it = std::find_if(RANGE(dstrb), [symb](auto& tup) {
      return std::get<0>(tup) == symb;
    });
    MY_ASSERT(it != dstrb.end());
    return *it;
  });
  return sample;
}

bool is_in_dstrb(const ProbDstrb_t& dstrb, symb_t symb) {
  auto it = std::find_if(RANGE(dstrb), [symb](auto& tup) {
    return std::get<0>(tup) == symb;
  });
  return it != dstrb.end();
}

