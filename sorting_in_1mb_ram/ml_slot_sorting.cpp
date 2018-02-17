#include <ml_slot_sorting.hpp>
#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <random>
#include <sstream>

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

MlSlotList::MlSlotList() { 
  for(auto& slots : l1_slots)
    slots.resize(l1_size, 0);
  for(auto& slots : l2_slots)
    slots.resize(l2_size, 0);
}

double MlSlotList::calculate_compaction() {
  auto pred = [](uint64_t acc, const std::vector<uint8_t>& vect) {
    bool last_empty = false;
    uint64_t cur_comp = 0, compact = 0;
    for(auto i : vect) {
      auto keep = (last_empty && is_empty(i)) || (!last_empty && not_empty(i));
      if(!keep)
        compact += cur_comp * cur_comp;
      cur_comp = keep ? cur_comp + 1 : 1;
      last_empty = is_empty(i);
    }
    return acc + compact + cur_comp * cur_comp;
  };

  auto compact = algo_acc(l1_slots, 0, pred);
  return 1.0 * compact / tot_fix_len;
}

MlSlotStats MlSlotList::calculate_stats() {
  MlSlotStats::l1_size_t l1_occ;
  MlSlotStats::l2_size_t l2_occ;
  size_t tot_l1 = 0, tot_l2 = 0;

  for(uint32_t i=0; i<l1_len; ++i) {
    l1_occ[i] = algo(count_if, l1_slots[i], not_empty);
    tot_l1 += l1_occ[i];
  }
  for(uint32_t i=0; i<l2_len; ++i) {
    l2_occ[i] = algo(count_if, l2_slots[i], not_empty);
    tot_l2 += l2_occ[i];
  }

  double wasted_perc = 100. * (tot_fix_len - tot_l1 - tot_l2) / tot_fix_len;
  MY_ASSERT(wasted_perc >= 0.0);

  MlSlotStats stats = {
    l1_occ,
    l2_occ,
    last_resort.size(),
    calculate_compaction(),
    wasted_perc,
  };
  return stats;
}

bool MlSlotList::add_number_l1(uint32_t number) {
  uint8_t  rem = number % l1_mult;
  uint32_t qot = number / l1_mult;

  auto it = algo(find_if, l1_slots, [&](std::vector<uint8_t>& v) {
    auto empty = is_empty(v[qot]);
    if(empty) v[qot] = rem + slot_min_val;
    return empty;
  });
  return it != l1_slots.end();
}

bool MlSlotList::add_number_l2(uint32_t number) {
  uint16_t rem = number % l2_mult;
  uint32_t qot = number / l2_mult;

  auto it = algo(find_if, l2_slots, [&](std::vector<uint16_t>& v) {
    auto empty = is_empty(v[qot]);
    if(empty) v[qot] = rem + slot_min_val;
    return empty;
  });
  return it != l2_slots.end();
}

void MlSlotList::add_number(uint32_t number) {
  auto add_ok = add_number_l1(number) || add_number_l2(number);
  if(!add_ok)
    last_resort.push_back(number + slot_min_val);
}

ItContainer<TreeIt> MlSlotList::range() {
  TreeIt begin{*this}, end{*this};
  end.buffer.clear();
  end.move_to_end();
  algo_srt(last_resort);
  ItContainer<TreeIt> begin_end{ begin, end };
  return begin_end;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

TreeIt::TreeIt(const MlSlotList& parent_) 
  : parent(parent_),
    l1_cur {{0}}, l2_cur{{0}}, last_cur{0} {
  // not accurate but highly likely
  for(int i=0; i<100 && buffer.empty(); ++i)
    fill_buffer();
  if (buffer.empty())
    move_to_end();
}

TreeIt::reference TreeIt::operator*() {
  return buffer.back();
}

TreeIt& TreeIt::operator++() {
  if(buffer.size() > 1)
    buffer.pop_back();
  else {
    buffer.clear();
    while(buffer.empty() && l1_cur[0] < l1_size)
      fill_buffer();
    if(buffer.empty())
      move_to_end();
  }
  return *this;
}

TreeIt TreeIt::operator++(int) {
  TreeIt it {*this};
  ++it;
  return it;
}

bool TreeIt::operator==(const TreeIt& rhs) const {
  auto l1_eq = algo(equal, l1_cur, rhs.l1_cur.begin());
  auto l2_eq = algo(equal, l2_cur, rhs.l2_cur.begin());
  auto buf_eq = buffer.size() == rhs.buffer.size();
  return l1_eq && l2_eq && buf_eq;
}

bool TreeIt::operator!=(const TreeIt& rhs) const { return !(*this == rhs); }

void TreeIt::move_to_end() {
  MY_ASSERT(buffer.empty());
  algo(fill, l1_cur, l1_size);
  algo(fill, l2_cur, l2_size);
  last_cur = -1;
}

void TreeIt::fill_buffer() {
  int row_idx = 0;
  std::transform(l1_cur.begin(), l1_cur.end(), std::back_inserter(buffer),
    [this, &row_idx](size_t& idx) { 
      const auto& row = parent.l1_slots[row_idx++];
      uint32_t val = slot_empty;
      if(idx < l1_size && row[idx] != slot_empty)
        val = (uint32_t)(idx * l1_mult + row[idx]);
      //LOG("row_idx=" << row_idx << " idx=" << idx << " val1=" << val);
      idx += idx < l1_size ? 1 : 0;
      return val;
  });

  row_idx = 0;
  auto ceil = l1_cur[0] * l1_mult + slot_min_val;

  std::transform(l2_cur.begin(), l2_cur.end(), std::back_inserter(buffer),
    [this, &row_idx, ceil](size_t& idx) { 
      const auto& row = parent.l2_slots[row_idx++];
      uint32_t val = slot_empty;
      if(idx < l2_size && row[idx] != slot_empty) {
        val = (uint32_t)(idx * l2_mult + row[idx]);
        //LOG("row_idx=" << row_idx << " idx=" << idx << " val=" << val);
      }
      if(val >= ceil)
        val = slot_empty;
      else {
        //LOG("row_idx=" << row_idx << " idx=" << idx << " val2=" << val);
        idx += idx < l2_size ? 1 : 0;
      }
      return val;
  });

  const auto& last = parent.last_resort;
  for(; last_cur < last.size() && last[last_cur] < ceil; ++last_cur)
    buffer.push_back(last[last_cur]);

  buffer.erase(algo(remove, buffer, slot_empty), buffer.end());
  algo(sort, buffer, std::greater<uint32_t>());
  std::transform(buffer.begin(), buffer.end(), buffer.begin(),
    [](uint32_t val) { return val - slot_min_val; }
  );
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<class T>
T my_format(T input) {
  return input;
}

template<class T, class V>
std::string my_format(const std::pair<T,V>& input) {
  std::stringstream ss;
  ss << "(" << input.first << "," << input.second << ")";
  return ss.str();
}

template<class V>
std::string print_collection(const V& input) {
  std::stringstream ss;
  ss << "[";
  for(auto i : input) ss << my_format(i) << ",";
  ss << "]";
  return ss.str();
}

std::string MlSlotStats::to_string() {
  std::stringstream ss;
  ss << "stats(" << std::endl;
  ss << " l1_sizes=" << print_collection(l1_sizes);
  ss << " l2_sizes=" << print_collection(l2_sizes);
  ss << " last_size=" << last_size 
      << " l1_compact=" << l1_compact 
      << " wasted=" << wasted_perc;
  ss << std::endl << ")";
  return ss.str();
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::vector<uint32_t> generate_rand_uint_input(uint32_t len, uint32_t max_item_val=max_v_mask) {
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dist(0, max_item_val);

  std::vector<uint32_t> input;
  input.reserve(len);
  for(uint32_t i=0; i<len; ++i)
    input.push_back(dist(gen));
  return input;
}

void sort_numbers_by_ml_slots() {
  MlSlotList container;
  auto input = generate_rand_uint_input(input_len);

  for(auto n : input)
    container.add_number(n);
  auto stats = container.calculate_stats();
  LOG(stats.to_string());

  auto sort_input = input;
  algo_srt(sort_input);
  size_t ref_idx = 0;
  auto dd = container.range();
  for(auto val : container.range()) {
    if(val != sort_input[ref_idx])
      LOG("at " << ref_idx << " : " << val << " != " << sort_input[ref_idx]);
    MY_ASSERT(val == sort_input[ref_idx]);
    ref_idx += 1;
  }
}

void check_8_bucket_compaction() {
  auto input = generate_rand_uint_input(8*1000000, 799);
  auto histo = std::map<uint32_t, uint32_t>();
  uint32_t can_fit = 0;

  for(int i=0; i<input.size(); i+=8) {
    std::array<uint32_t, 8> arr {{input[i+0], input[i+1], input[i+2], input[i+3], input[i+4], input[i+5], input[i+6], input[i+7], }};
    algo_srt(arr);
    int violations = 0;
    algo_acc(arr, 0, [&](uint32_t prev, uint32_t cur) {
      if(cur - prev > 255) violations++;
      return cur;
    });
    if(!violations) ++can_fit;
    else ++histo[violations];
  }
  LOG("check_8_bucket_compaction can_fit=" << can_fit << " histo=" << print_collection(histo));
}

void check_4_bucket_compaction() {
  auto input = generate_rand_uint_input(4*1000000, 399);
  uint32_t can_fit = 0;

  for(int i=0; i<input.size(); i+=4) {
    std::array<uint32_t, 4> arr {{input[i+0], input[i+1], input[i+2], input[i+3], }};
    algo_srt(arr);
    if(arr[0] < 64 && arr[0]) ++can_fit;
    MY_ASSERT(arr[0] < arr[3]);
  }
  LOG("check_4_bucket_compaction can_fit=" << can_fit);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(void) {
  sort_numbers_by_ml_slots();
  //check_8_bucket_compaction();
  //check_4_bucket_compaction();
  return 0;
}

