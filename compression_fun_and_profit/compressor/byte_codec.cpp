#include <cinttypes>

#include <byte_codec.hpp>

void byte_compress_numbers(const std::vector<delta_t>& input, std::vector<uint8_t>& output) {
  for(auto number : input) {
    if(number >= -byte_codec_lvl1() && number <= byte_codec_lvl1()) {
      output.resize(output.size() + 2, 0);
      auto buffer = &(output.back()) - 1;
      byte_encode_small(number, buffer);
    }
    else if(number >= -byte_codec_lvl2() && number <= byte_codec_lvl2()) {
      output.resize(output.size() + 4, 0);
      auto buffer = &(output.back()) - 3;
      byte_encode_medium(number, buffer);
    }
    else {
      output.resize(output.size() + sizeof(delta_t) + 1, 0);
      auto buffer = &(output.back()) - sizeof(delta_t);
      byte_encode_large(number, buffer);
    }
  }
}

void byte_decompress_numbers(const std::vector<uint8_t>& input, std::vector<delta_t>& output) {
  constexpr size_t MASK_SMALL = (1 << PREFIX_SMALL_LEN) - 1;
  constexpr size_t MASK_MEDIUM = (1 << PREFIX_MEDIUM_LEN) - 1;
  auto data = input.data();

  for(uint32_t i=0,j=0; i<input.size(); ++j) {
    if((input[i] & MASK_SMALL) == PREFIX_SMALL) {
      output[j] = byte_decode_small(data+i);
      i += 2;
    }
    else if((input[i] & MASK_MEDIUM) == PREFIX_MEDIUM) {
      output[j] = byte_decode_medium(data+i);
      i += 4;
    }
    else {
      output[j] = byte_decode_large(data+i);
      i += 5;
    }
  }
}

template<class A>
delta_t recompose(A a) { return a; }
template<class A, class ... K>
delta_t recompose(A a, K ... k) { return a + ALPHA_LEN * recompose(k...); }

delta_t byte_decode_small(const uint8_t* buffer) {
  int8_t a = (buffer[0] >> PREFIX_SMALL_LEN) - ALPHA_LEN;
  int8_t b = (buffer[1] & 0x7f) - ALPHA_LEN;
  int8_t c = (buffer[1] >> 7);
  int8_t sign = ((uint8_t)a >> 7) ? -1 : 1;
  c = c * sign;
  a = ((a + ALPHA_LEN) ? a : 0);
  return recompose(a, b, c);
}

delta_t byte_decode_medium(const uint8_t* buffer) {
  constexpr size_t M = 2*ALPHA_LEN - 1;
  auto encoded = *reinterpret_cast<const uint32_t*>(buffer) >> PREFIX_MEDIUM_LEN;

  int8_t a = (encoded & M) - ALPHA_LEN;
  int8_t b = ((encoded >> 7) & M) - ALPHA_LEN;
  int8_t c = ((encoded >> 14) & M) - ALPHA_LEN;
  int8_t d = ((encoded >> 21) & M) - ALPHA_LEN;
  int8_t e = (encoded >> 28);
  int8_t sign = ((uint8_t)a >> 7) ? -1 : 1;
  e = (e * sign);
  a = ((a + ALPHA_LEN) ? a : 0);
  return recompose(a, b, c, d, e);
}

delta_t byte_decode_large(const uint8_t* buffer) {
  return *reinterpret_cast<const delta_t*>(buffer+1);
}

std::array<int32_t, 5> decompose_in_base(delta_t number) {
  std::array<int32_t, 5> result {};
  delta_t sign = (number < 0) ? -1 : 1;
  number = std::abs(number);
  for(auto& coef : result) {
    coef = sign * (number % ALPHA_LEN);  
    number /= ALPHA_LEN;
  }
  MY_ASSERT(!number);
  return result;
}

void byte_encode_small(delta_t number, uint8_t* buffer) {
  auto decomp = decompose_in_base(number);
  decomp[0] = (decomp[0] == 0 && number < 0)? -ALPHA_LEN : decomp[0];
  buffer[0] = ALPHA_LEN + decomp[0];
  buffer[0] = (buffer[0] << PREFIX_SMALL_LEN) | PREFIX_SMALL;
  buffer[1] = ALPHA_LEN + decomp[1];
  buffer[1] |= (!!decomp[2]) << 7;
}

void byte_encode_medium(delta_t number, uint8_t* buffer) {
  auto decomp = decompose_in_base(number);
  decomp[0] = (decomp[0] == 0 && number < 0)? -ALPHA_LEN : decomp[0];
  auto& as_int = *reinterpret_cast<uint32_t*>(buffer);
  as_int = PREFIX_MEDIUM
         | ((ALPHA_LEN + decomp[0]) << PREFIX_MEDIUM_LEN)
         | ((ALPHA_LEN + decomp[1]) << (PREFIX_MEDIUM_LEN + 7))
         | ((ALPHA_LEN + decomp[2]) << (PREFIX_MEDIUM_LEN + 14))
         | ((ALPHA_LEN + decomp[3]) << (PREFIX_MEDIUM_LEN + 21))
         | (std::abs(decomp[4])     << (PREFIX_MEDIUM_LEN + 28));
}

void byte_encode_large(delta_t number, uint8_t* buffer) {
  buffer[0] = PREFIX_LARGE;
  auto as_int = reinterpret_cast<delta_t*>(buffer+1);
  *as_int = number;
}

//////////////////////////////////////////////////////////////////////////////////////////////

std::unique_ptr<Compressed> byte_compress_from_delta(const Series& input) {
  constexpr size_t CHUNK_LEN = 8192;

  auto output = std::make_unique<Compressed>(DFormat::BYTE_COMP, input.count());
  for(uint32_t i=0; i<input.count(); ++i) {
    output->meta[i] = input.meta[i];
    output->data[i].reserve(CHUNK_LEN);
    byte_compress_numbers(input.data[i], output->data[i]);
    MY_ASSERT(output->data[i].size() > 0 || input.meta[i].count == 0);
  }
  return output;
}

std::unique_ptr<Series> delta_from_byte_compress(const Compressed& input) {
  auto output = std::make_unique<Series>(DFormat::DELTA, input.count());
  for(uint32_t i=0; i<input.count(); ++i) {
    output->meta[i] = input.meta[i];
    output->data[i].resize(input.meta[i].count);
    byte_decompress_numbers(input.data[i], output->data[i]);
    MY_ASSERT(output->data[i].size() > 0 || input.meta[i].count == 0);
  }
  return output;
}

