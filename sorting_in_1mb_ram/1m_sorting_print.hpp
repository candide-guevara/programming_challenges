#pragma once
#include <iostream>
#include <sstream>
#include <iomanip>
#pragma GCC diagnostic ignored "-Wunused-function"

#define LOG(msg) std::cout << msg << std::endl;
//#define STRAT_LOG(num, msg) std::cout << "strategy " << num << " : " << msg << std::endl;
#define STRAT_LOG(num, msg) 
#define IOMANIPS std::scientific << std::setprecision(7) << std::dec
#define TEST_HEADER() LOG(__PRETTY_FUNCTION__ << " start")

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

template<class T, class V>
std::string my_format(const std::pair<T,V>& input) {
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

std::string my_format(const Buckets& buckets) {
  auto ss = build_stringstream();
  for(uint32_t idx=0; idx < bucket_len; ++idx)
    ss << idx << " : start=" << (void*)buckets.starts[idx] << " end=" << (void*)buckets.ends[idx]
       << " lens=" << (buckets.lens[idx] / 8) << "/" << (buckets.lens[idx] % 8) 
       << " cap=" << (buckets.ends[idx] - buckets.starts[idx])
       << " start+lens=" << (void*)(buckets.starts[idx] + buckets.lens[idx]/8) << std::endl;
  return ss.str();
}

std::string my_format(const Buckets& buckets, uint32_t idx) {
  auto ss = build_stringstream();
  if(idx < bucket_len)
    ss << idx << " : start=" << (void*)buckets.starts[idx] << " end=" << (void*)buckets.ends[idx]
       << " lens=" << (buckets.lens[idx] / 8) << "/" << (buckets.lens[idx] % 8) 
       << " cap=" << (buckets.ends[idx] - buckets.starts[idx])
       << " start+lens=" << (void*)(buckets.starts[idx] + buckets.lens[idx]/8);
  else
    ss << "out of bounds";
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

std::string print_stats(const StatBuckets& stats) {
  auto ss = build_stringstream();
  ss << "{ min_cap=" << my_format(stats.min_cap) << " min_avail=" << my_format(stats.min_avail);
  ss << " max_cap=" <<  my_format(stats.max_cap) << " max_avail=" << my_format(stats.max_avail) << std::endl;
  ss << " avg_avail=" << stats.avg_avail;
  ss << " std_cap=" << stats.std_cap << " std_avail=" << stats.std_avail << std::endl;
  ss << " tot_avail=" << stats.tot_avail << " tot_len=" << stats.tot_len << std::endl;
  ss << " len_histo=" << print_collection(stats.len_histo) << std::endl;
  ss << " val_histo=" << print_collection(stats.val_histo) << std::endl;
  ss << " }" << std::endl;
  return ss.str();
}

} //namespce anon

