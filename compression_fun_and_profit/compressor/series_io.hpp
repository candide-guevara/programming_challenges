#pragma once
#include <cstdint>
#include <memory>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

#include <common.hpp>

#define ASSERT_POD(type) static_assert(std::is_pod<type>::value, #type " is not POD")

enum class DFormat : uint16_t {
  RAW = 0,
  NORMAL = 1,
  DELTA = 2,
};

enum class FFormat : uint16_t {
  UNKNOWN = 0,
};

struct FileHeader {
  FFormat fformat; 
  DFormat dformat;
  uint32_t count;
};
ASSERT_POD(FileHeader);

struct SeriesMetadata {
  uint64_t sid;
  double min, max;
  uint32_t start, count;
};
ASSERT_POD(SeriesMetadata);

struct Series {
  DFormat dformat;
  std::vector<SeriesMetadata> meta;
  std::vector<std::vector<delta_t>> data;
  
  explicit Series(DFormat format, uint32_t count);
  size_t count() { return data.size(); }
};

using ProbTuple = std::tuple<symb_t, prob_t, prob_t>;
inline symb_t symbol(const ProbTuple& t) { return std::get<0>(t); }
inline prob_t cumsum(const ProbTuple& t) { return std::get<1>(t); }
inline prob_t weight(const ProbTuple& t) { return std::get<2>(t); }
using ProbDstrb = std::vector<ProbTuple>;

std::unique_ptr<Series> read_series_from_file(std::string filepath);
std::unique_ptr<ProbDstrb> read_prob_dstrb_from_file(std::string filepath);
std::string prod_to_string(const ProbDstrb& dstrb);

