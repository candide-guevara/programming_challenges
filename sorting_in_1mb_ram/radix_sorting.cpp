#include <radix_sorting.hpp>
#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <random>
#include <sstream>

RadixTree::RadixTree() 
  : chunk(lvl1_len, slot_empty) { } 

uint32_t RadixTree::add_number(uint32_t number) {
  auto slot = (number >> lvl1_shf);
  auto payload = (number & lvl1_mask);
  MY_ASSERT(slot < lvl1_len);

  if(chunk[slot] == slot_empty)
    chunk[slot] = (payload | lvl0_flag);
  else if(chunk[slot] & lvl0_flag) {
    auto previous = (chunk[slot] & lvl0_mask);
    auto id = next_lvl.add_number(previous);
    chunk[slot] = id;
    id = next_lvl.add_number(id, number);
    MY_ASSERT(id != slot_empty && id == chunk[slot] && !(chunk[slot] & lvl0_flag));
  }
  else {
    auto id = next_lvl.add_number(chunk[slot], number);
    if(id == slot_empty) {
      last_resort.push_back(number);
      return slot_empty;
    }
    MY_ASSERT(id == chunk[slot] && !(chunk[slot] & lvl0_flag));
  }
  return slot_min_val;
}

ItContainer<RadixLvl1It> RadixTree::range() {
  RadixIt begin{}, end{};
  algo_srt(last_resort);

  begin.last_resort_it = last_resort.begin();
  begin.chunk_begin = begin.chunk_it = chunk.begin();
  begin.last_resort_end = end.last_resort_end = last_resort.end();
  begin.chunk_end = end.chunk_end = chunk.end();

  while(*(begin.chunk_it) == slot_empty && begin.chunk_it != chunk_end)
    ++begin.chunk_it;
  if(begin.chunk_it != chunk_end) {
    auto next_range = next_lvl.range(*(begin.chunk_it));
    begin.next_it = next_range.begin();
    begin.next_end = end.next_end = next_range.end();
  }
  else {
    auto next_range = next_lvl.range(slot_empty);
    begin.next_end = end.next_end = next_range.end();
    begin.move_to_end();
  }

  end.move_to_end();
  ItContainer<RadixIt> begin_end{ begin, end };
  return begin_end;
}

uint32_t RadixIt::extract(uint32_t number) {
  auto dist = std::distance(chunk_begin, chunk_it);
  return (dist << lvl0_shf) + number;
}

reference RadixIt::operator*() {
  if(next_it != next_end) {
    auto next_val = extract(*next_it);
    if(last_resort_it != last_resort_end) {
      if(*last_resort_it < next_val) return *last_resort;
      else return next_val;
    }
    else return next_val;
  }
  MY_ASSERT(last_resort_it != last_resort_end); 
  return *last_resort;
}

RadixIt&  RadixIt::operator++() {
  auto cur = **this;
  if(last_resort_it != last_resort_end && cur == *last_resort_it) 
    ++last_resort_it;
  else if(next_it != next_end && cur == *next_it) {
    ++next_it;
    if(next_it == next_end) {
      ++chunk_it;
      next_it = RadixLvl1It()
    }
  }
  return *this;
}

RadixIt   RadixIt::operator++(int) {
  RadixIt it {*this};
  ++it;
  return it;
}

bool RadixIt::operator==(const RadixIt& rhs) const {
  return last_resort_it == rhs.last_resort_it
    && next_it == rhs.next_it
    && chunk_it == rhs.chunk_it;
}

bool RadixIt::operator!=(const RadixIt& rhs) const { return !(*this == rhs); }

void RadixIt::move_to_end() {
  last_resort_it = last_resort_end;
  next_it = next_end;
  chunk_it = chunk_end;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

RadixLevel1::RadixLevel1() {}

uint32_t RadixLevel1::add_number(uint32_t number) {
  RadixLevel2 lvl2;
  auto id = lvl2.add_number(number);
  MY_ASSERT(id != slot_empty);
  chunks.push_back(lvl2);
  return chunks.size() - 1 + slot_min_val;
}

uint32_t RadixLevel1::add_number(uint32_t id, uint32_t number) {
  id -= slot_min_val;
  MY_ASSERT(id < chunks.size());
  auto id = chunks[id].add_number(number);
  return id;
}

ItContainer<RadixLvl1It> RadixLevel1::range(uint32_t id) {
  RadixLvl2It next_it, next_end;
  RadixLvl1It begin{}, end{};

  begin.id = end.id = id;
  begin.consumed = end.consumed = false;

  if(!(id & lvl0_flag)) { 
    id -= slot_min_val;
    auto next_range = chunks[id].range();
    begin.next_it = next_range.begin();
    begin.next_end = end.next_end = next_range.end();
  }

  end.move_to_end();
  ItContainer<RadixLvl1It> begin_end{ begin, end };
  return begin_end;
}

reference     RadixLvl1It::operator*() {
  if(id & lvl0_flag) 
    return (id & lvl0_mask);
  return *next_it;
}

RadixLvl1It&  RadixLvl1It::operator++() {
  if(id & lvl0_flag) {
    MY_ASSERT(!consumed);
    consumed = true;
  }
  else ++next_it;
  return *this;
}

RadixLvl1It   RadixLvl1It::operator++(int) {
  RadixLvl1It it {*this};
  ++it;
  return it;
}

bool RadixLvl1It::operator==(const RadixIt& rhs) const {
  return ((id & lvl0_flag) && consumed == rhs.consumed && id == rhs.id)
    || (next_it == rhs.next_it);
}

bool RadixLvl1It::operator!=(const RadixIt& rhs) const { return !(*this == rhs); }

void RadixLvl1It::move_to_end() {
  consumed = true;
  next_it = next_end;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

RadixLevel2::RadixLevel2() 
  : chunk {{slot_empty}} {}

uint32_t RadixLevel2::add_number(uint32_t number) {
  auto slot = (number & lvl0_mask) >> lvl2_shf;
  auto payload = (number & lvl2_mask);

  if(chunk[slot] == slot_empty)
    chunk[slot] = (payload | lvl2_flag);
  else if(chunk[slot] & lvl2_flag) {
    auto previous = (chunk[slot] & lvl2_mask);
    auto id = next_lvl.add_number(previous);

    if(id == slot_empty) return slot_empty;

    chunk[slot] = id;
    id = next_lvl.add_number(id, number);
    MY_ASSERT(id == chunk[slot] && !(chunk[slot] & lvl2_flag));
  }
  else {
    auto id = next_lvl.add_number(chunk[slot], number);
    if(id == slot_empty) return slot_empty;
    MY_ASSERT(id == chunk[slot] && !(chunk[slot] & lvl2_flag));
  }
  return slot_min_val;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

RadixLevel3::RadixLevel3() {}

uint32_t RadixLevel3::add_number(uint32_t number) {
  if (chunks.size() >= lvl2_mask)
    return slot_empty;
  std::array<uint8_t, lvl3_len> chunk {{slot_empty}};
  auto payload = (number & lvl2_mask);
  chunk[0] = payload;
  chunks.push_back(chunk);
  return chunks.size() - 1 + slot_min_val;
}

uint32_t RadixLevel3::add_number(uint32_t id, uint32_t number) {
  id -= slot_min_val;
  MY_ASSERT(id < chunks.size());
  auto payload = (number & lvl2_mask);

  auto& chunk = chunks[id];
  auto it = std::find(chunk.begin(), chunk.end(), slot_empty);
  if (it != chunk.end()) {
    *it = (payload | lvl2_flag);
    return id;
  }
  return slot_empty;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void sort_numbers_by_radix() {}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(void) {
  sort_numbers_by_radix();
  return 0;
}

