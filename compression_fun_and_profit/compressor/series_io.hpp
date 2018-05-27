#pragma once
#include <cstdint>
#include <memory>
#include <numeric>
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
  BYTE_COMP = 3,
  BIT_COMP = 4,
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

#define MetaBaseFields \
  uint64_t sid;        \
  double min, max;     \
  uint32_t start;      \

struct CompMetadata {
  MetaBaseFields
};
ASSERT_POD(CompMetadata);

struct SeriesMetadata {
  MetaBaseFields
  uint32_t count;
};
ASSERT_POD(SeriesMetadata);

template<class T>
struct SeriesTmpl {
  DFormat dformat;
  using data_type = typename T::value_type;
  std::vector<SeriesMetadata> meta;
  std::vector<T> data;
  
  explicit SeriesTmpl(DFormat format, uint32_t count)
    : dformat(format)
    , meta(count)
    , data(count) {}

  size_t count() const { return data.size(); }
  size_t data_bytes() const { return std::accumulate(RANGE(data), 0, [](auto acc, const auto& s) { return acc + s.size() * sizeof(data_type); }); }
  size_t meta_bytes() const { return sizeof(SeriesMetadata) * meta.size(); }
  size_t total_bytes() const { return sizeof(DFormat) + data_bytes() + meta_bytes(); }
};
using Series = SeriesTmpl<std::vector<delta_t>>;
using Compressed = SeriesTmpl<std::vector<uint8_t>>;

using ProbTuple = std::tuple<symb_t, prob_t, prob_t>;
inline symb_t symbol(const ProbTuple& t) { return std::get<0>(t); }
inline prob_t cumsum(const ProbTuple& t) { return std::get<1>(t); }
inline prob_t weight(const ProbTuple& t) { return std::get<2>(t); }
using ProbDstrb_t = std::vector<ProbTuple>;

std::unique_ptr<Series> read_series_from_file(std::string filepath);
std::unique_ptr<ProbDstrb_t> read_prob_dstrb_from_file(std::string filepath);
void dump_compressed_to_file(const Compressed& comp, std::string filename);

std::string prob_to_string(const ProbDstrb_t& dstrb);
std::string seriesmeta_to_string(const SeriesMetadata& meta);
std::string series_to_string(const Series& series);

