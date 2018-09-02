#include <byte_codec.hpp>
#include <series_io.hpp>
#include <test.hpp>

#include <algorithm>
#include <functional>

void print_comp_stats(std::string name, const Series& series, const Compressed& comp) {
  auto data_ratio = 1.0*series.data_bytes() / comp.data_bytes();
  auto total_ratio = 1.0*series.total_bytes() / comp.total_bytes();
  LOG(name << " : data_ratio=" << data_ratio << ", total_ratio=" << total_ratio
      << ", data=" << series.data_bytes() << ", total=" << series.total_bytes() );
}

void compress_real_series() {
  std::string names[] = {"secout_bics_1_tech", "secout_xchng_us"};
  for(auto& name : names) {
    auto series = read_series_from_file(name + ".bin");
    auto dstrb = read_prob_dstrb_from_file(name + ".prob");
    auto byte_comp = byte_compress_from_delta(*series);
    auto bit_comp = bit_compress_from_delta(*series, *dstrb);

    dump_compressed_to_file(*byte_comp, name + ".byte.comp");
    dump_compressed_to_file(*bit_comp, name + ".bit.comp");
    LOG("byte compression :");
    print_comp_stats(name, *series, *byte_comp);
    LOG("bit compression :");
    print_comp_stats(name, *series, *bit_comp);
  }
}

int main(int argc, char** argv) {
  //test_all();
  compress_real_series();
  return 0;
}

