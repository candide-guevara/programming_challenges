#include <algorithm>
#include <bit_codec.hpp>

std::array<int32_t, PROB_BASE_LEN+1> decompose_and_collapse_tails(delta_t number) {
  std::array<int32_t, PROB_BASE_LEN+1> result {};
  delta_t sign = (number < 0) ? -1 : 1;
  number = std::abs(number);

  for(uint32_t i=0; i<PROB_BASE_LEN; ++i) {
    result[i] = sign * (number % ALPHA_LEN);  
    number /= ALPHA_LEN;
  }
  MY_ASSERT(number < static_cast<int32_t>(MAX_PROB / MAX_SYMB));
  result[PROB_BASE_LEN] = sign * number;
  return result;
}

///////////////////////////////////////////////////////////////////////////////////////////////////

void BitStream::write_bit(uint8_t bit) {
  auto bit_pos = write_pos % 8;
  if(!bit_pos)
    buffer.push_back(0);
  auto& back = buffer.back();
  back |= ((bit & 1) << bit_pos);
  ++write_pos;
}

prob_t BitStream::read_bit() {
  MY_ASSERT(read_pos < buffer.size() * 8);
  auto idx = read_pos / 8;
  auto bit_pos = read_pos % 8;
  ++read_pos;
  return (buffer[idx] >> bit_pos) & 1;
}

///////////////////////////////////////////////////////////////////////////////////////////////////

Interval::Interval(const ProbDstrb& _prob)
    : buffer{}, prob(_prob), min(0), max(MAX_PROB) {
  buffer.buffer.reserve(BitStream::INIT_LEN);
}

void Interval::move_comp_data(std::vector<uint8_t>& dest) {
  dest = std::move(buffer.buffer);
  buffer = BitStream{};
  min = 0;
  max = MAX_PROB;
  buffer.buffer.reserve(BitStream::INIT_LEN);
}

void Interval::lerp_by_prob(symb_t coef) {
  auto it = std::lower_bound(RANGE(prob), coef,
    [](const auto& lhs, auto rhs) { return std::get<0>(lhs) < rhs; } );
  MY_ASSERT(it != prob.end() && std::get<0>(*it) == coef);

  auto a0 = std::get<1>(*it) - std::get<2>(*it);
  auto a1 = std::get<2>(*it);
  min = a0 + min * a1 / MAX_PROB;
  max = a0 + (max * a1) / MAX_PROB;
  MY_ASSERT(min < max && max < MAX_PROB);
}

///////////////////////////////////////////////////////////////////////////////////////////////////

void Interval::write_number(delta_t number) {
  auto decomp = decompose_and_collapse_tails(number);
  symb_t exp = 1;
  for(uint32_t i=0; i<PROB_BASE_LEN; ++i) {
    write_coef(decomp[i] * exp, 1);
    exp *= ALPHA_LEN;
  }
  MY_ASSERT(exp == MAX_SYMB);
  write_coef(exp, decomp[PROB_BASE_LEN]);
}

void Interval::write_end_marker() {
  write_coef(END_MARKER, 1);
}

void Interval::write_coef(symb_t coef, size_t times) {
  while(times--) {
    lerp_by_prob(coef);
    expand_and_write_simple();
    expand_hard();
  }
}

void Interval::expand_and_write_simple() {
  while(((min ^ max) & LAST_PROB_DIGIT_MASK) == 0) {
    buffer.write_bit(!!(min & LAST_PROB_DIGIT_MASK));
    min <<= 1;
    max <<= 1;
  }
  min &= MAX_PROB_MASK;
  max &= MAX_PROB_MASK;
  MY_ASSERT(min < max && max < MAX_PROB);
}

void Interval::expand_hard() {
  constexpr prob_t THRESHOLD = MAX_PROB / 4;
  constexpr prob_t MID_POINT = MAX_PROB / 2;
  while(max - min < THRESHOLD) {
    MY_ASSERT(min <= MID_POINT && max >= MID_POINT && min != max);
    min = 2 * min - MID_POINT;
    max = 2 * max - MID_POINT;
  }
  MY_ASSERT(min < max && max < MAX_PROB);
}

///////////////////////////////////////////////////////////////////////////////////////////////////

delta_t Interval::read_number() {
}

symb_t Interval::read_symbol() {
  auto scaled = lerp_prob_dstrb();
  auto it_start = scaled.begin();
  auto it_end = scaled.end();
  uint32_t bit_pos = MAX_PROB_EXP;
  prob_t tmp_min=0, tmp_max=MAX_PROB;

  while(std::distance(it_start, it_end) > 0) {
    tmp_min |= (buffer.read_bit() << --bit_pos);
    tmp_max = (tmp_min | ((1llu << bit_pos) - 1));
    it_start = std::lower_bound(it_start, it_end, tmp_min);
    it_end = std::lower_bound(it_start, it_end, tmp_max);
  }

  MY_ASSERT(it_start == it_end && bit_pos);
  min = tmp_min;
  max = tmp_max;
}

///////////////////////////////////////////////////////////////////////////////////////////////////

std::unique_ptr<Compressed> bit_compress_from_delta(const Series& input, const ProbDstrb& prob) {
  auto interval = Interval(prob);
  auto output = std::make_unique<Compressed>(DFormat::BIT_COMP, input.count());

  for(uint32_t i=0; i<input.count(); ++i) {
    output->meta[i] = input.meta[i];
    for(auto number : input.data[i])
      interval.write_number(number);
    interval.write_end_marker();
    interval.move_comp_data(output->data[i]);
    MY_ASSERT(output->data[i].size() > 0 || input.meta[i].count == 0);
  }
  return output;
}

std::unique_ptr<Series> delta_from_bit_compress(const Compressed& input, const ProbDstrb& prob) {
  return std::unique_ptr<Series>();
}

