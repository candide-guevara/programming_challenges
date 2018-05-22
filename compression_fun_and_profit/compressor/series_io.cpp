#include <series_io.hpp>
#include <fstream>
#include <sstream>
#include <regex>

template<class T>
T read_pod(std::ifstream& fin) {
  static_assert(std::is_pod<T>::value, "");
  T pod{};
  fin.read((char*)&pod, sizeof(T));
  MY_ASSERT(fin);
  return pod;
}

template<class P>
std::string_view view_from_it_pair(const P& pair) {
  auto len = std::distance(pair.first, pair.second);
  auto view = std::string_view(&*(pair.first), len);
  return view;
}

template<class S>
prob_t str_to_int(S view) {
  return std::atoll(view.data());
}

std::tuple<std::string_view, std::string_view, std::string_view>
parse_prob_line(const std::string& line, const std::regex& rx) {
  std::smatch match;
  if(!std::regex_match(line, match, rx))
    throw std::runtime_error("line does not match : " + line);
  auto tup = std::make_tuple(view_from_it_pair(match[1]), view_from_it_pair(match[2]), view_from_it_pair(match[3]));
  return tup;
}

std::unique_ptr<Series> read_series_from_file(std::string filepath) {
  auto fin = std::ifstream(filepath, std::ios::binary);
  MY_ASSERT(fin);

  auto header = read_pod<FileHeader>(fin);
  MY_ASSERT(header.count && header.dformat == DFormat::DELTA);
  auto series = std::make_unique<Series>(header.dformat, header.count);

  for(size_t i=0; i<header.count; ++i) {
    auto& meta = series->meta[i];
    auto& data = series->data[i];
    meta = read_pod<SeriesMetadata>(fin);
    data.resize(meta.count);
    fin.read((char*)data.data(), meta.count * sizeof(delta_t));
  }
  MY_ASSERT(fin.get() == std::ifstream::traits_type::eof());
  return series;
}

std::unique_ptr<ProbDstrb> read_prob_dstrb_from_file(std::string filepath) {
  auto fin = std::ifstream(filepath, std::ios::binary);
  MY_ASSERT(fin);

  auto dstrb = std::make_unique<ProbDstrb>();
  dstrb->reserve(256);
  std::string line;
  std::regex line_rx("(.*),(.*),(.*)", std::regex::extended);

  while(std::getline(fin, line)) {
    if(line.empty() || line[0] == COMMENT) continue;

    auto [sym,cum,w] = parse_prob_line(line, line_rx);
    if(sym == "None")
      dstrb->emplace_back(END_MARKER, str_to_int(cum), str_to_int(w));
    else
      dstrb->emplace_back(str_to_int(sym), str_to_int(cum), str_to_int(w));
  }
  return dstrb;
}

void dump_compressed_to_file(const Compressed& comp, std::string filename) {
  auto fout = std::ofstream(filename, std::ios::binary | std::ios::out);
  MY_ASSERT(fout);
  FileHeader header = { FFormat::UNKNOWN, DFormat::BYTE_COMP, static_cast<uint32_t>(comp.count()) };
  fout.write(reinterpret_cast<const char*>(&header), sizeof(FileHeader));
  for(uint32_t i=0; i<comp.count(); ++i) {
    auto& meta = comp.meta[i];
    auto& data = comp.data[i];
    fout.write(reinterpret_cast<const char*>(&meta), sizeof(SeriesMetadata));
    fout.write(reinterpret_cast<const char*>(data.data()), data.size() * sizeof(Compressed::data_type));
  }
  MY_ASSERT(fout);
}

std::string prob_to_string(const ProbDstrb& dstrb) {
  auto buf = std::stringstream{};
  for(auto [sym,cum,w] : dstrb)
    buf << sym << "," << cum << "," << w << std::endl;
  return buf.str();
}

std::string seriesmeta_to_string(const SeriesMetadata& meta) {
  auto buf = std::stringstream{};
  buf << "sid=" << meta.sid 
      << "min=" << meta.min << "max=" << meta.max 
      << "start=" << meta.start << "count=" << meta.count;
  return buf.str();
}

std::string series_to_string(const Series& series) {
  auto buf = std::stringstream{};
  for(uint32_t i=0; i<series.count(); ++i) {
    auto& meta = series.meta[i];
    auto& data = series.data[i];
    buf << seriesmeta_to_string(meta) << std::endl;
    for(uint32_t j=0; j<(data.size() < 10 ? data.size() : 10); ++j)    
      buf << data[j] << std::endl;
  }
  return buf.str();
}

