#include <radix_sorting.hpp>
#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <random>
#include <sstream>

RadixTree::RadixTree() 
    : chunk(lvl0_len, slot_empty) { } 

uint32_t RadixTree::to_chunk_payload(uint32_t number) const {
    auto payload = (number >> lvl0_shf) + slot_min_val;
    MY_ASSERT(payload < lvl0_flag);
    return (payload | lvl0_flag);
}

uint32_t RadixTree::from_chunk_payload(uint32_t slot) const {
    auto number = (chunk[slot] & (~lvl0_flag)) - slot_min_val;
    number = (number << lvl0_shf) + slot;
    MY_ASSERT(number <= max_number);
    return number;
}

uint32_t RadixTree::add_to_last_resort(uint32_t number) {
    auto end = last_resort.end();
    auto it = algo(lower_bound, last_resort, number);
    if(it != end)
        std::move_backward(it, end-1, end);

    *it = number;
    return slot_min_val;
}

uint32_t RadixTree::add_number(uint32_t number) {
    MY_ASSERT(number <= max_number);
    auto slot = (number & lvl0_mask);

    if(chunk[slot] == slot_empty) {
        chunk[slot] = to_chunk_payload(number);
        return slot_min_val;
    }
    else if(chunk[slot] & lvl0_flag) {
        auto previous = from_chunk_payload(slot);
        auto id = next_lvl.add_number(previous);
        chunk[slot] = id;
        MY_ASSERT(id != slot_empty && !(chunk[slot] & lvl0_flag));
    }

    auto id = next_lvl.add_number(chunk[slot], number);
    if(id == slot_empty) {
        add_to_last_resort(number);
        return slot_empty;
    }
    return slot_min_val;
}

ItContainer<RadixIt> RadixTree::range() const {
    RadixIt begin{this, 
        last_resort.begin(),
        chunk.begin(),
    };
    begin.advance_chunk_skip_empty(false);

    RadixIt end{this};
    end.move_to_end();

    ItContainer<RadixIt> begin_end{ begin, end };
    return begin_end;
}

uint32_t RadixIt::extract_direct() const {
    auto dist = std::distance(parent->chunk.begin(), chunk_it);
    return parent->from_chunk_payload(dist);
}

uint32_t RadixIt::extract_lower_lvl() const {
    auto dist = std::distance(parent->chunk.begin(), chunk_it);
    auto number = (*next_it << lvl0_shf) + dist;
    MY_ASSERT(number <= max_number);
    return number;
}

RadixIt::reference RadixIt::operator*() const {
    if(chunk_it != parent->chunk.end() && (*chunk_it & lvl0_flag)) {
        auto res = extract_direct();
        MY_ASSERT(next_it == parent->next_lvl.empty_it());
        MY_ASSERT(last_resort_it != parent->last_resort.end() || *last_resort_it >= res); 
        return res;
    }

    if(next_it != next_end) {
        auto next_val = extract_lower_lvl();
        if(last_resort_it == parent->last_resort.end() || *last_resort_it > next_val) 
            return next_val;
    }

    MY_ASSERT(next_it != next_end || next_it == parent->next_lvl.empty_it());
    MY_ASSERT(last_resort_it != parent->last_resort.end()); 
    return *last_resort_it;
}

void RadixIt::advance_chunk_skip_empty(bool preincrement) {
    if(preincrement) ++chunk_it;
    while(chunk_it != parent->chunk.end() && *chunk_it == slot_empty)
        ++chunk_it;

    if(chunk_it != parent->chunk.end()) {
        if(*chunk_it & lvl0_flag) {
            next_it  = parent->next_lvl.empty_it();
            next_end = parent->next_lvl.empty_it();
        }
        else {
            auto next_range = parent->next_lvl.range(*chunk_it);
            next_it = next_range.begin();
            next_end = next_range.end();
        }
    }
    else move_to_end();
}

RadixIt& RadixIt::operator ++ () {
    if(chunk_it != parent->chunk.end() && (*chunk_it & lvl0_flag)) {
        MY_ASSERT(next_it == parent->next_lvl.empty_it());
        advance_chunk_skip_empty();    
    }

    if(next_it != next_end) {
        MY_ASSERT(chunk_it != parent->chunk.end());
        auto next_val = extract_lower_lvl();

        if(last_resort_it == parent->last_resort.end() || *last_resort_it > next_val)
            advance_chunk_skip_empty();    
    }

    MY_ASSERT(next_it != next_end || next_it == parent->next_lvl.empty_it());
    MY_ASSERT(last_resort_it != parent->last_resort.end()); 
    ++last_resort_it;
    return *this;
}

RadixIt RadixIt::operator++(int) {
    RadixIt it {*this};
    ++it;
    return it;
}

bool RadixIt::operator==(const RadixIt& rhs) const {
    MY_ASSERT(parent == rhs.parent);
    return last_resort_it == rhs.last_resort_it
        && next_it == rhs.next_it
        && chunk_it == rhs.chunk_it;
}

bool RadixIt::operator!=(const RadixIt& rhs) const { return !(*this == rhs); }

void RadixIt::move_to_end() {
    last_resort_it = parent->last_resort.end();
    chunk_it = parent->chunk.end();
    next_it = next_end = parent->next_lvl.empty_it();
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

RadixLevel1::RadixLevel1() {}

uint32_t RadixLevel1::add_number(uint32_t number) {
    chunks.emplace_back();
    MY_ASSERT(chunks.size() < lvl0_len);
    auto id = chunks.back().add_number(number);
    MY_ASSERT(id != slot_empty);
    return chunks.size() - 1 + slot_min_val;
}

uint32_t RadixLevel1::add_number(uint32_t id, uint32_t number) {
    id -= slot_min_val;
    MY_ASSERT(id < chunks.size());
    id = chunks[id].add_number(number);
    return id;
}

RadixLvl1It RadixLevel1::empty_it() const { 
    return RadixLvl1It{this, lvl0_len}; 
}

ItContainer<RadixLvl1It> RadixLevel1::range(uint32_t id) const {
    id -= slot_min_val;
    MY_ASSERT(id < chunks.size());   

    auto next_range = chunks[id].range();
    MY_ASSERT(next_range.begin() != next_range.end());
    RadixLvl1It begin{this, id, next_range.begin(), next_range.end()};
    RadixLvl1It end{begin};
    end.move_to_end();

    ItContainer<RadixLvl1It> begin_end{ begin, end };
    return begin_end;
}

RadixLvl1It::reference RadixLvl1It::operator*() const {
    MY_ASSERT(next_it != next_end && id < lvl0_len);
    return *next_it;
}

RadixLvl1It& RadixLvl1It::operator++() {
    MY_ASSERT(next_it != next_end && id < lvl0_len);
    ++next_it;
    return *this;
}

RadixLvl1It RadixLvl1It::operator++(int) {
    RadixLvl1It it {*this};
    ++it;
    return it;
}

bool RadixLvl1It::operator==(const RadixLvl1It& rhs) const {
    MY_ASSERT(parent == rhs.parent);
    return (id == rhs.id && id == lvl0_len)
        || (id == rhs.id && next_it == rhs.next_it);
}

bool RadixLvl1It::operator!=(const RadixLvl1It& rhs) const { return !(*this == rhs); }

void RadixLvl1It::move_to_end() {
    MY_ASSERT(id < lvl0_len);
    next_it = next_end;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

RadixLevel2::RadixLevel2() 
    : chunk {{slot_empty}}, extra{{slot_empty}}, extra_top{} {}

uint32_t RadixLevel2::to_chunk_payload(uint32_t number) const {
    auto payload = (number >> (lvl2_shf + lvl0_shf)) + slot_min_val;
    MY_ASSERT(payload < lvl2_flag);
    return payload;
}

uint32_t RadixLevel2::to_extra_payload(uint32_t number) const {
    auto payload = (number >> lvl0_shf) + slot_min_val;
    MY_ASSERT(payload < lvl0_flag);
    return payload;
}

uint32_t RadixLevel2::add_number(uint32_t number) {
    auto slot = (number >> lvl0_shf) & lvl2_mask;

    if(chunk[slot] == slot_empty) {
        auto payload = to_chunk_payload(number);
        chunk[slot] = payload;
        return slot_min_val;
    }
    auto id = add_to_extra(number);
    return id;
}

uint32_t RadixLevel2::add_to_extra(uint32_t number) {
    if(extra_top == extra.size())
        return slot_empty;

    auto payload = to_extra_payload(number);
    auto end = extra.begin() + extra_top;
    auto it = std::lower_bound(extra.begin(), end, payload);
    if(it != end)
        std::move_backward(it, end-1, end);

    *it = payload;
    ++extra_top;
    return slot_min_val;
}

ItContainer<RadixLvl2It> RadixLevel2::range() const {
    RadixLvl2It begin{this, chunk.begin(), extra.begin()};
    begin.advance_chunk_skip_empty(false);
    RadixLvl2It end{this};
    end.move_to_end();

    MY_ASSERT(begin.chunk_it != chunk.end() 
        || begin.extra_it == extra.begin() + extra_top);
    ItContainer<RadixLvl2It> begin_end{ begin, end };
    return begin_end;
}

void RadixLvl2It::advance_chunk_skip_empty(bool preincrement) {
    if(preincrement) ++chunk_it;
    while(chunk_it != parent->chunk.end() && *chunk_it == slot_empty)
        ++chunk_it;
}

uint32_t RadixLvl2It::extract_from_chunk() const {
    auto dist = std::distance(parent->chunk.begin(), chunk_it);
    auto result = ((*chunk_it - slot_min_val) << lvl2_shf) + dist;
    MY_ASSERT(result < lvl0_flag);
    return result; 
}

uint32_t RadixLvl2It::extract_from_extra() const {
    auto result = *extra_it - slot_min_val;
    MY_ASSERT(result < lvl0_flag);
    return result; 
}

RadixLvl2It::reference RadixLvl2It::operator*() const {
    auto extra_end = parent->extra.begin() + parent->extra_top;
    auto result = slot_empty;

    if(chunk_it != parent->chunk.end()) {
        auto result = extract_from_chunk();
        if(extra_it != extra_end)
            result = std::max(extract_from_extra(), result);
    }
    else if(extra_it != extra_end)
        result = extract_from_extra();
    else MY_ASSERT(false);
    return result;
}

RadixLvl2It& RadixLvl2It::operator++() {
    auto extra_end = parent->extra.begin() + parent->extra_top;

    if(chunk_it != parent->chunk.end()) {
        auto chunk_val = extract_from_chunk();
        if(extra_it != extra_end && chunk_val > extract_from_extra())
            ++extra_it;
        else advance_chunk_skip_empty();
    }
    else if(extra_it != extra_end)
        ++extra_it;
    else MY_ASSERT(false);
    return *this;
}

RadixLvl2It RadixLvl2It::operator++(int) {
    RadixLvl2It it {*this};
    ++it;
    return it;
}

bool RadixLvl2It::operator==(const RadixLvl2It& rhs) const {
    MY_ASSERT(parent == rhs.parent && parent->extra_top == rhs.parent->extra_top);
    return chunk_it == rhs.chunk_it && extra_it == rhs.extra_it;
}

bool RadixLvl2It::operator!=(const RadixLvl2It& rhs) const { return !(*this == rhs); }

void RadixLvl2It::move_to_end() {
    auto extra_end = parent->extra.begin() + parent->extra_top;
    chunk_it = parent->chunk.end();
    extra_it = extra_end;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void RadixStats::calculate_on(RadixTree& level) {
    free_lv0 = algo(count_if, level.chunk, [](uint32_t v) { return v == slot_empty; });
    take_lv0 = algo(count_if, level.chunk, [](uint32_t v) { return v & lvl0_flag; });
    chld_lv0 = lvl0_len - take_lv0 - free_lv0;
    calculate_on(level.next_lvl);
}

void RadixStats::calculate_on(RadixLevel1& level) {
    size_lv1 = level.chunks.size();
    for(RadixLevel2& next : level.chunks)
        calculate_on(next);
}

void RadixStats::calculate_on(RadixLevel2& level) {
    free_lv2 += algo(count_if, level.chunk, [](uint32_t v) { return v == slot_empty; });
    size_lv2 += lvl2_len - free_lv2;
    perc_lv2 += 100. * size_lv2 / (size_lv1 * lvl2_len);

    free_lv3 += algo(count_if, level.extra, [](uint32_t v) { return v == slot_empty; });
    size_lv3 += lvl2_xtr - free_lv3;
    perc_lv3 += 100. * size_lv3 / (size_lv1 * lvl2_xtr);
}

std::string RadixStats::to_string() const {
    std::stringstream ss;
    ss << "stats(" << std::endl;
    ss << " free_lv0=" << free_lv0
        << " take_lv0=" << take_lv0
        << " chld_lv0=" << chld_lv0 << std::endl;
    ss << " size_lv1=" << size_lv1 << std::endl;
    ss << " size_lv2=" << size_lv2
        << " free_lv2=" << free_lv2
        << " perc_lv2=" << perc_lv2 << std::endl;
    ss << " size_lv3=" << size_lv3
        << " free_lv3=" << free_lv3
        << " perc_lv3=" << perc_lv3 << std::endl;
    ss << ")";
    return ss.str();
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::vector<uint32_t> generate_rand_uint_input(uint32_t len, uint32_t max_item_val=max_number) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist(0, max_item_val);

    std::vector<uint32_t> input;
    input.reserve(len);
    for(uint32_t i=0; i<len; ++i)
        input.push_back(dist(gen));
    return input;
}

void sort_numbers_by_radix() {
    RadixTree container;
    auto input = generate_rand_uint_input(input_len);
    for(auto n : input)
        container.add_number(n);

    RadixStats stats {};
    stats.calculate_on(container);
    LOG(stats.to_string());

    auto sort_input = input;
    algo_srt(sort_input);
    size_t ref_idx = 0;
    for(auto val : container.range()) {
        if(val != sort_input[ref_idx])
            LOG("at " << ref_idx << " : " << val << " != " << sort_input[ref_idx]);
        MY_ASSERT(val == sort_input[ref_idx]);
        ref_idx += 1;
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(void) {
    sort_numbers_by_radix();
    return 0;
}

