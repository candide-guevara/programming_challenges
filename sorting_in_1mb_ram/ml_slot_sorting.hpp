#pragma once
#include <array>
#include <cassert>
#include <iterator>
#include <vector>

#pragma GCC diagnostic ignored "-Wunused-function"
#define LOG(msg) std::cout << msg << std::endl;
#define algo(name, v, pred) std::name(v.begin(), v.end(), pred)
#define algo_acc(v, init, pred) std::accumulate(v.begin(), v.end(), init, pred)
#define algo_srt(v) std::sort(v.begin(), v.end())

#ifdef NDEBUG
#define MY_ASSERT(x) do { (void)sizeof(x); } while (0)
#else
#define MY_ASSERT(x) assert(x)
#endif

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

const static size_t input_len  = 1000000;
const static size_t max_v_mask = 100000000;
const static size_t l1_size = 400000;
const static size_t l1_len = 2;
const static size_t l1_mult = max_v_mask / l1_size;
const static size_t l2_size = 1600;
const static size_t l2_len = 75;
const static size_t l2_mult = max_v_mask / l2_size;
const static size_t tot_fix_len = l2_len * l2_size + l1_size * l1_len;

const static uint32_t slot_empty = 0;
const static uint32_t slot_min_val = 1;

struct MlSlotList;


template<class I>
struct ItContainer {
  I start_it;
  I end_it;
  I begin() const { return start_it; }
  I end() const { return end_it; }
};

struct TreeIt {
  using l1_cur_t = std::array<size_t, l1_len>;
  using l2_cur_t = std::array<size_t, l2_len>;
  const MlSlotList& parent;
  l1_cur_t l1_cur;
  l2_cur_t l2_cur;
  size_t last_cur;
  std::vector<uint32_t> buffer;

  using value_type = uint32_t;
  using difference_type = int32_t;
  using pointer = const uint32_t*;
  using reference = const uint32_t&;
  using iterator_category = std::forward_iterator_tag;

  TreeIt(const MlSlotList& parent);
  reference operator*();
  TreeIt& operator++();
  TreeIt operator++(int);

  bool operator==(const TreeIt& rhs) const;
  bool operator!=(const TreeIt& rhs) const;

  void fill_buffer();
  void move_to_end();
};

struct MlSlotStats {
  using l1_size_t = std::array<size_t, l1_len>;
  using l2_size_t = std::array<size_t, l2_len>;
  l1_size_t l1_sizes;
  l2_size_t l2_sizes;
  size_t last_size;
  double l1_compact, wasted_perc;

  std::string to_string();
};

struct MlSlotList {
  using l1_slot_t = std::array<std::vector<uint8_t>, l1_len>;
  using l2_slot_t = std::array<std::vector<uint16_t>, l2_len>;
  l1_slot_t l1_slots;
  l2_slot_t l2_slots;
  std::vector<uint32_t> last_resort;

  MlSlotList();

  void add_number   (uint32_t);
  bool add_number_l1(uint32_t);
  bool add_number_l2(uint32_t);
  bool has_number(uint32_t);

  MlSlotStats calculate_stats();
  double calculate_compaction();
  ItContainer<TreeIt> range();
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

inline bool is_empty(uint32_t number)  { return number == slot_empty; }
inline bool not_empty(uint32_t number) { return number != slot_empty; }

