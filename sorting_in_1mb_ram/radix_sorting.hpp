#pragma once
#include <array>
#include <cassert>
#include <iterator>
#include <list>
#include <vector>

#pragma GCC diagnostic ignored "-Wunused-function"
#define LOG(msg) std::cout << msg << std::endl;

#ifdef NDEBUG
#define MY_ASSERT(x) do { (void)sizeof(x); } while (0)
#else
#define MY_ASSERT(x) assert(x)
#endif

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

const static size_t input_len  = 1000000;
const static size_t max_v_mask = 100000000;
const static size_t lvl0_len   = 4096;
const static size_t lvl0_shf   = 15;
const static size_t lvl0_mask  = (1 << lvl0_shf) - 1;
const static size_t lvl0_flag  = 1 << lvl0_shf;
const static size_t lvl2_len   = 256;
const static size_t lvl2_shf   = 7;
const static size_t lvl2_mask  = (1 << lvl2_shf) - 1;
const static size_t lvl2_flag  = 1 << lvl2_shf;
const static size_t lvl3_len   = 4;

const static uint32_t slot_empty = 0;
const static uint32_t slot_min_val = 1;

template<class I>
struct ItContainer {
  I start_it;
  I end_it;
  I begin() const { return start_it; }
  I end() const { return end_it; }
};

struct RadixLvl3It {};
struct RadixLvl2It {};

struct RadixLvl1It {
  uint32_t id;
  bool consumed;
  RadixLvl2It next_it, next_end;

  using value_type = uint32_t;
  using difference_type = int32_t;
  using pointer = const uint32_t*;
  using reference = const uint32_t&;
  using iterator_category = std::forward_iterator_tag;

  reference operator*();
  RadixIt&  operator++();
  RadixIt   operator++(int);

  bool operator==(const RadixIt& rhs) const;
  bool operator!=(const RadixIt& rhs) const;
  void move_to_end();
};

struct RadixIt {
  std::vector<uint32_t>::const_iterator last_resort_it, last_resort_end;
  std::vector<uint16_t>::const_iterator chunk_it, chunk_begin, chunk_end;
  RadixLvl1It next_it, next_end;

  using value_type = uint32_t;
  using difference_type = int32_t;
  using pointer = const uint32_t*;
  using reference = const uint32_t&;
  using iterator_category = std::forward_iterator_tag;

  reference operator*();
  RadixLvl1It& operator++();
  RadixLvl1It operator++(int);

  bool operator==(const RadixIt& rhs) const;
  bool operator!=(const RadixIt& rhs) const;
  void move_to_end();
  uint32_t extract(uint32_t number);
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct RadixLevel3 {
  std::list<std::array<uint8_t, lvl3_len>> chunks;
  uint32_t add_number(uint32_t number);
  uint32_t add_number(uint32_t id, uint32_t number);
  ItContainer<RadixLvl3It> range();
};

struct RadixLevel2 {
  std::array<uint8_t, lvl2_len> chunk;
  RadixLevel3 next_lvl;
  uint32_t add_number(uint32_t number);
  ItContainer<RadixLvl2It> range();
};

struct RadixLevel1 {
  std::list<RadixLevel2> chunks;
  RadixLevel1();
  uint32_t add_number(uint32_t number);
  uint32_t add_number(uint32_t id, uint32_t number);
  ItContainer<RadixLvl1It> range(uint32_t id);
};

struct RadixTree {
  std::vector<uint32_t> last_resort;
  std::vector<uint16_t> chunk;
  RadixLevel2 next_lvl;

  RadixTree();
  uint32_t add_number(uint32_t number);
  ItContainer<RadixLvl1It> range();
};

