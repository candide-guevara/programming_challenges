#pragma once
#include <iostream>
#include <sstream>
#include <iomanip>
#pragma GCC diagnostic ignored "-Wunused-function"

#define LOG(msg) std::cout << msg << std::endl;
//#define STRAT_LOG(num, msg) std::cout << "strategy " << num << " : " << msg << std::endl;
#define STRAT_LOG(num, msg) 
#define IOMANIPS std::scientific << std::setprecision(7) << std::dec

namespace {

inline std::stringstream build_stringstream() {
  std::stringstream ss;
  ss << IOMANIPS;
  return ss;
}

template<class T>
T my_format(T input) {
  return input;
}

template<class T>
std::string my_format(const std::pair<T,T>& input) {
  auto ss = build_stringstream();
  ss << "(" << input.first << "," << input.second << ")";
  return ss.str();
}

inline uint32_t my_format(uint8_t n) {
  return (uint32_t)n;
}

inline void* my_format(uint8_t* p) {
  return (void*)p;
}

std::string my_format(const StatBuckets& stats) {
  auto ss = build_stringstream();
  ss << "{ min_cap=" << my_format(stats.min_cap) << " min_avail=" << my_format(stats.min_avail);
  ss << " max_cap=" <<  my_format(stats.max_cap) << " max_avail=" << my_format(stats.max_avail) << std::endl;
  ss << " avg_avail=" << stats.avg_avail;
  ss << " std_cap=" << stats.std_cap << " std_avail=" << stats.std_avail << std::endl;
  ss << " tot_avail=" << stats.tot_avail << " tot_len=" << stats.tot_len ;
  ss << " }" << std::endl;
  return ss.str();
}

template<class V>
std::string print_collection(const V& input) {
  auto ss = build_stringstream();
  ss << "[";
  for(auto i : input) ss << my_format(i) << ",";
  ss << "]";
  return ss.str();
}

std::string print_raw_bucket(const uint8_t* start, size_t len) {
  auto ss = build_stringstream();
  ss << (void*)start << "=[";
  for(const uint8_t* i=start; (size_t)(i-start)<len; ++i)
    ss << (uint32_t)(*i) << ",";
  ss << "]";
  return ss.str();
}


} //namespce anon

