#pragma once
#include <array>
#include <cassert>
#include <iterator>
#include <list>
#include <string>
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
const static size_t max_number = 100000000;
const static size_t bits_len   = 27;
const static size_t lvl0_shf   = 14;
const static size_t lvl0_cap   = (max_number >> lvl0_shf) + 1;
const static size_t lvl0_flg1  = 1 << lvl0_shf;
const static size_t lvl0_flg2  = 1 << (1 + lvl0_shf);
const static size_t lvl0_allf  = lvl0_flg1 + lvl0_flg2;
const static size_t lvl0_mask  = lvl0_flg1 - 1;
const static size_t lvl2_shf   = 6;
const static size_t lvl2_cap   = 1 << (lvl0_shf - lvl2_shf);
const static size_t lvl2_flg1  = 1 << lvl2_shf;
const static size_t lvl2_flg2  = 1 << (1 + lvl2_shf);
const static size_t lvl2_allf  = lvl2_flg1 + lvl2_flg2;
const static size_t lvl2_mask  = lvl2_flg1 - 1;
const static size_t lvl2_xtr   = 8;

const static uint32_t slot_empty = 0;
const static uint32_t add_ok = 1;
const static uint32_t max_offset = 3;

template<class I>
struct ItContainer {
    I start_it;
    I end_it;
    I begin() const { return start_it; }
    I end() const { return end_it; }
};

struct RadixTree;
struct RadixLevel1;
struct RadixLevel2;

struct RadixLvl2It {
    using chunk_t = std::array<uint8_t, lvl2_cap>;
    using extra_t = std::array<uint16_t, lvl2_xtr>;
    
    const RadixLevel2* parent;
    chunk_t::const_iterator chunk_it;
    extra_t::const_iterator extra_it;

    using value_type = uint32_t;
    using difference_type = int32_t;
    using pointer = const uint32_t*;
    using reference = const uint32_t;
    using iterator_category = std::input_iterator_tag;

    reference operator*() const;
    RadixLvl2It& operator++();
    RadixLvl2It  operator++(int);

    bool operator==(const RadixLvl2It& rhs) const;
    bool operator!=(const RadixLvl2It& rhs) const;
    void move_to_end();
    void advance_chunk_skip_empty(bool=true);
    uint32_t extract_from_extra() const;
};

struct RadixLvl1It {
    const RadixLevel1* parent;
    uint32_t id;
    RadixLvl2It next_it, next_end;

    using value_type = uint32_t;
    using difference_type = int32_t;
    using pointer = const uint32_t*;
    using reference = const uint32_t;
    using iterator_category = std::input_iterator_tag;

    reference operator*() const;
    RadixLvl1It& operator++();
    RadixLvl1It  operator++(int);

    bool operator==(const RadixLvl1It& rhs) const;
    bool operator!=(const RadixLvl1It& rhs) const;
    void move_to_end();
};

struct RadixIt {
    const RadixTree* parent;
    std::vector<uint32_t>::const_iterator last_resort_it;
    std::vector<uint16_t>::const_iterator chunk_it;
    RadixLvl1It next_it, next_end;

    using value_type = uint32_t;
    using difference_type = int32_t;
    using pointer = const uint32_t*;
    using reference = const uint32_t;
    using iterator_category = std::input_iterator_tag;

    reference operator*() const;
    RadixIt& operator++();
    RadixIt  operator++(int);

    bool operator==(const RadixIt& rhs) const;
    bool operator!=(const RadixIt& rhs) const;
    void move_to_end();
    void advance_chunk_skip_empty(bool=true);
    uint32_t extract_direct() const;
    uint32_t extract_lower_lvl() const;
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct RadixLevel2 {
    RadixLvl2It::chunk_t chunk;
    RadixLvl2It::extra_t extra;
    uint8_t extra_top;

    RadixLevel2();
    uint32_t add_number(uint32_t number);
    uint32_t add_to_extra(uint32_t number);
    void reorder_after_offset(uint32_t slot, uint32_t offset);
    ItContainer<RadixLvl2It> range() const;
    uint32_t to_chunk_payload(uint32_t number, uint32_t offset) const;
    uint32_t to_extra_payload(uint32_t number) const;
    uint32_t extract_from_chunk(RadixLvl2It::chunk_t::const_iterator it) const;
    uint32_t slot_from_number(uint32_t number) const;
};

struct RadixLevel1 {
    std::vector<RadixLevel2> chunks;

    RadixLevel1();
    uint32_t add_number(uint32_t number);
    uint32_t add_number(uint32_t id, uint32_t number);
    ItContainer<RadixLvl1It> range(uint32_t id) const;
    RadixLvl1It empty_it() const;
};

struct RadixTree {
    std::vector<uint32_t> last_resort;
    std::vector<uint16_t> chunk;
    RadixLevel1 next_lvl;

    RadixTree();
    uint32_t add_number(uint32_t number);
    uint32_t add_to_last_resort(uint32_t number);
    ItContainer<RadixIt> range() const;
    uint32_t to_chunk_payload(uint32_t number) const;
    uint32_t from_chunk_payload(uint32_t slot) const;
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct RadixStats {
    uint32_t free_lv0, take_lv0, chld_lv0;
    uint32_t size_lst;
    uint32_t size_lv2, free_lv2; 
    double perc_lv2;
    uint32_t size_lv3, free_lv3; 
    double perc_lv3;

    void calculate_on(RadixTree&);
    void calculate_on(RadixLevel1&);
    void calculate_on(RadixLevel2&);
    /// this does not count lvl1 because it if you ar smart you can avoid to allocate a vector
    size_t total_size_bytes() const;
    std::string to_string() const;
};
