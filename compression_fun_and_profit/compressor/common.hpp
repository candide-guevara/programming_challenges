#pragma once
#include <iostream>
#include <cassert>
#include <cstdint>
#include <cstdlib>

#ifdef NDEBUG
#define MY_ASSERT(x) do { const bool __b__ = false && (x); (void)sizeof(__b__); } while (0)
#define TEST_ASSERT(exp)                                                                                               \
  if (!(exp)) {                                                                                                        \
    LOG("TEST_ASSERT : " #exp);                                                                                        \
    throw std::runtime_error("test_failed");                                                                           \
  }
#else
#define MY_ASSERT(x) assert(x)
#define TEST_ASSERT(x) assert(x)
#endif

#define LOG(msg) std::cerr << __PRETTY_FUNCTION__ << "[" << __LINE__ << "] " << msg << std::endl

#define RANGE(coll) (coll).begin(), (coll).end()

using delta_t = int32_t;
using symb_t = int32_t;
using prob_t = uint64_t;

constexpr prob_t MAX_PROB_EXP = 32;
constexpr prob_t MAX_PROB = (1ull << MAX_PROB_EXP) - 1;
constexpr prob_t MAX_PROB_HIGH_BIT = 1ull << (MAX_PROB_EXP - 1);
constexpr size_t ALPHA_LEN = 64;
constexpr size_t PROB_BASE_LEN = 4;
constexpr char COMMENT = '#';

constexpr symb_t A_POW(size_t p) {
  symb_t max = 1;
  for(uint32_t i=0; i<p; ++i)
    max *= ALPHA_LEN;
  return max;
}
constexpr symb_t __MAX_SYMB__() {
  symb_t max = 1;
  for(uint32_t i=0; i<PROB_BASE_LEN; ++i)
    max *= ALPHA_LEN;
  return max;
}
constexpr symb_t MAX_SYMB = __MAX_SYMB__();
constexpr symb_t END_MARKER = MAX_SYMB - 1;

