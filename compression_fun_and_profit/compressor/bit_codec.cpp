#include <algorithm>
#include <iterator>
#include <sstream>

#include <bit_codec.hpp>

BitWriter::BitWriter() {
  write_pos = 8;
  buffer.reserve(INIT_LEN);
  buffer.push_back(0);
}

void BitWriter::write_bit(uint8_t bit) {
  auto& back = buffer.back();
  back |= ((bit & 1) << --write_pos);
  if(write_pos == 0) {
    buffer.push_back(0);
    write_pos = 8;
  }
}

void BitWriter::skip_to_next_byte() {
  buffer.push_back(0);
  write_pos = 8;
}

std::string BitWriter::buffer_to_str() {
  auto buf = std::stringstream{};
  buf << std::hex << "[ ";
  std::ostream_iterator<uint32_t> out_it(buf, ", ");
  std::copy(RANGE(buffer), out_it);
  buf << " ]";
  return buf.str();
}

void BitReader::load_data(const uint8_t* start) {
  MY_ASSERT(start);
  read_pos = 8;
  buffer = start;
}

uint64_t BitReader::read_bit() {
  auto result = ((*buffer >> --read_pos) & 1);
  if(read_pos == 0) {
    buffer += 1;
    read_pos = 8;
  }
  return result;
}

///////////////////////////////////////////////////////////////////////////////////////////////////

Decomp_t decompose_and_collapse_tails(delta_t number) {
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

BitEncoder::BitEncoder(const ProbDstrb_t& prob) {
  build_symbol_table(prob);
}

void BitEncoder::build_symbol_table(const ProbDstrb_t& prob) {
  MY_ASSERT(symb_tab.size() == 0);
  symb_tab.reserve(prob.size() * 2);

  for(auto [symb, cum, delt] : prob) {
    auto base_shft = encoding_for_range(cum-delt, cum);
    MY_ASSERT(std::all_of(RANGE(symb_tab), [base_shft](auto& kv) { return kv.second != base_shft; }));
    symb_tab[symb] = base_shft;
  }
  MY_ASSERT(symb_tab.size() == prob.size());
}

SymbTable_t::mapped_type BitEncoder::encoding_for_range(prob_t min, prob_t max) {
  prob_t base = 0;
  size_t shift = 0;

  for(auto bit_msk = MAX_PROB_HIGH_BIT; ((min ^ max) & bit_msk) == 0 && bit_msk;) {
    prob_t next = (min & bit_msk) ? 1 : 0;
    base = ((base << 1) | next); 
    bit_msk >>= 1;
    shift += 1;
  }
  base <<= 1;
  shift += 1;
  for(size_t unshift=MAX_PROB_EXP-shift; (base << unshift) < min;) {
    base = ((base << 1) | 1);
    unshift -= 1;
    shift += 1;
  }

  auto result = SymbTable_t::mapped_type{base, shift};
  MY_ASSERT(result.first < MAX_PROB && result.second < MAX_PROB_EXP);
  return result;
}

void BitEncoder::move_comp_data(std::vector<uint8_t>& dest) {
  dest = std::move(buffer.buffer);
  buffer = BitWriter{};
}

///////////////////////////////////////////////////////////////////////////////////////////////////

void BitEncoder::write_number(delta_t number) {
  auto decomp = decompose_and_collapse_tails(number);
  symb_t exp = 1;
  for(auto coef : decomp) {
    // every number will start with a coef in [-ALPHA_LEN+1, ALPHA_LEN-1]
    // but other null coef will be omitted
    if(coef && exp == MAX_SYMB) 
      write_coef(number < 0 ? -MAX_SYMB : MAX_SYMB, std::abs(coef));
    else if(coef || exp == 1)
      write_coef(coef * exp, 1);
    exp *= ALPHA_LEN;
  }
}

void BitEncoder::write_end_marker() {
  write_coef(END_MARKER, 1);
}

void BitEncoder::write_coef(symb_t coef, size_t times) {
  auto [base, shift] = symb_tab.at(coef);
  MY_ASSERT(times < MAX_SYMB);
  while(times-- > 0) {
    auto tmp_shift = shift;
    while(tmp_shift-- > 0)
      buffer.write_bit((base >> tmp_shift) & 1); 
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

BitDecoder::BitDecoder(const ProbDstrb_t& prob) {
  build_cumsum(prob);
  build_idx_to_symb(prob);
  build_symb_to_rank(prob);
}

void BitDecoder::load_data_and_prime_carry(const uint8_t* start) {
  // we prime the carry
  buffer.load_data(start);
  carry = read_symbol();
}

void BitDecoder::build_symb_to_rank(const ProbDstrb_t& prob) {
  MY_ASSERT(symb_to_rank.size() == 0);
  symb_to_rank.reserve(prob.size() * 2);
  auto out_it = std::inserter(symb_to_rank, symb_to_rank.end());
  std::transform(RANGE(prob), out_it, [](auto& t) { 
    auto symb = std::abs(std::get<0>(t)); 
    SymbRank_t::mapped_type rank = 0;
    while(symb /= ALPHA_LEN) ++rank;
    MY_ASSERT(rank <= PROB_BASE_LEN);
    return SymbRank_t::value_type{std::get<0>(t), rank};
  });
  MY_ASSERT(symb_to_rank.size() == prob.size());
}

void BitDecoder::build_idx_to_symb(const ProbDstrb_t& prob) {
  MY_ASSERT(idx_to_symb.size() == 0);
  idx_to_symb.reserve(prob.size());
  auto out_it = std::back_inserter(idx_to_symb);
  std::transform(RANGE(prob), out_it, [](auto& t) { return std::get<0>(t); });
  MY_ASSERT(idx_to_symb.size() == prob.size());
}

void BitDecoder::build_cumsum(const ProbDstrb_t& prob) {
  MY_ASSERT(cumsum.size() == 0);
  cumsum.reserve(prob.size());
  auto out_it = std::back_inserter(cumsum);
  std::transform(RANGE(prob), out_it, [](auto& t) { return std::get<1>(t); });
  MY_ASSERT(cumsum.size() == prob.size() && cumsum.back() == MAX_PROB);
}

///////////////////////////////////////////////////////////////////////////////////////////////////

delta_t BitDecoder::read_number() {
  if(carry == END_MARKER) return END_MARKER;
  MY_ASSERT(symb_to_rank.find(carry) != symb_to_rank.end());
  MY_ASSERT(symb_to_rank.at(carry) == 0);

  delta_t result = 0;
  while(true) {
    result += carry;
    carry = read_symbol();
    auto rank = symb_to_rank.at(carry);

    if(carry == END_MARKER || rank == 0)
      break;
  }
  return result;
}

symb_t BitDecoder::read_symbol() {
  auto it_start = cumsum.begin();
  auto it_end = cumsum.end();
  uint32_t bit_pos = MAX_PROB_EXP;
  prob_t min = 0, max = 0;

  while(std::distance(it_start, it_end)) {
    min |= (buffer.read_bit() << --bit_pos);
    max = min | ((1llu << bit_pos) - 1llu);
    it_start = std::lower_bound(it_start, it_end, min);
    it_end = std::upper_bound(it_start, it_end, max);
  }

  MY_ASSERT(bit_pos && it_start != cumsum.end());
  auto result = idx_to_symb.at(std::distance(cumsum.begin(), it_start));
  return result;
}

///////////////////////////////////////////////////////////////////////////////////////////////////

std::unique_ptr<Compressed> bit_compress_from_delta(const Series& input, const ProbDstrb_t& prob) {
  auto encoder = BitEncoder(prob);
  auto output = std::make_unique<Compressed>(DFormat::BIT_COMP, input.count());

  for(uint32_t i=0; i<input.count(); ++i) {
    output->meta[i] = input.meta[i];
    for(auto number : input.data[i])
      encoder.write_number(number);
    encoder.write_end_marker();
    encoder.move_comp_data(output->data[i]);
    MY_ASSERT(output->data[i].size() > 0 || input.meta[i].count == 0);
  }
  return output;
}

std::unique_ptr<Series> delta_from_bit_compress(const Compressed& input, const ProbDstrb_t& prob) {
  constexpr size_t INIT_LEN = 365 * 5;
  auto decoder = BitDecoder(prob);
  auto output = std::make_unique<Series>(DFormat::DELTA, input.count());

  for(uint32_t i=0; i<input.count(); ++i) {
    auto& meta = output->meta[i];
    auto& data = output->data[i];

    meta = input.meta[i];
    data.reserve(INIT_LEN);
    decoder.load_data_and_prime_carry(input.data[i].data());

    for(auto number = decoder.read_number(); 
        number != END_MARKER; 
        number = decoder.read_number())
      data.push_back(number);

    meta.count = data.size();
    MY_ASSERT(meta.count);
  }
  return output;
}

