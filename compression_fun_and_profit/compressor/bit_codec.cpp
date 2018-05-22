#include <bit_io.hpp>

void Interval::write_bit(uint8_t bit) {
  auto bit_pos = buffer.write_pos % 8;
  if(!bit_pos)
    buffer.buffer.push_back(0);
  auto& back = buffer.buffer.back();
  back |= ((bit & 1) << bit_pos);
  ++buffer.write_pos;
}

uint8_t Interval::read_bit() {
  MY_ASSERT(buffer.read_pos < buffer.buffer.size() * 8);
  auto idx = buffer.read_pos / 8;
  auto bit_pos = buffer.read_pos % 8;
  ++buffer.read_pos;
  return (buffer.buffer[idx] >> bit_pos) & 1;
}

void Interval::write_number(delta_t number) {
  auto decomp = decompose(number);
  for(auto& [coef, times] : decomp) {
  }
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

void Interval::lerp_by_prob(symb_t coef) {
  auto it = std::lower_bound(RANGE(prob), coef,
    [] (auto& lhs, auto& rhs) { return std::get<0>(lhs) < std::get<0>(rhs); } );
  MY_ASSERT(it != prob.end() && std::get<0>(*it) == coef);

  auto a0 = std::get<1>(*it) - std::get<2>(*it);
  auto a1 = std::get<2>(*it);
  min = a0 + min * a1 / MAX_PROB;
  max = a0 + max * a1 / MAX_PROB;
  MY_ASSERT(min < max && max < MAX_PROB);
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
  constexpr THRESHOLD = MAX_PROB / 4;
  constexpr MID_POINT = MAX_PROB / 2;
  while(max - min < THRESHOLD) {
    MY_ASSERT(min <= MID_POINT && max <= MID_POINT && min != max);
    min = 2 * min - MID_POINT;
    max = 2 * max - MID_POINT;
  }
  MY_ASSERT(min < max && max < MAX_PROB);
}

std::unique_ptr<Compressed> bit_compress_from_delta(const Series& input, const ProbDstrb& prob) {
}

std::unique_ptr<Series> delta_from_bit_compress(const Compressed& input, const ProbDstrb& prob) {
}

