#pragma once
#include <iostream>
#include <cassert>
#include <cstdint>
#include <cstdlib>

#ifdef NDEBUG
#define MY_ASSERT(x) do { (void)sizeof(x); } while (0)
#else
#define MY_ASSERT(x) assert(x)
#endif

#define TEST_ASSERT(x) assert(x)

#define LOG(msg) std::cerr << __PRETTY_FUNCTION__ << "[" << __LINE__ << "] " << msg << std::endl

#define RANGE(coll) (coll).begin(), (coll).end()

using delta_t = int32_t;
using symb_t = int32_t;
using prob_t = uint64_t;

constexpr symb_t END_MARKER = -1 >> 1;
constexpr prob_t MAX_PROB = 1ull << 32;
constexpr prob_t MAX_PROB_MASK = MAX_PROB - 1;
constexpr prob_t LAST_PROB_DIGIT_MASK = MAX_PROB >> 1;
constexpr size_t ALPHA_LEN = 64;
constexpr char COMMENT = '#';

