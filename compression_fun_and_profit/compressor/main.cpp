#include <byte_codec.hpp>
#include <series_io.hpp>
#include <test.hpp>

#include <algorithm>
#include <functional>

void byte_compress_real_series() {
  std::string names[] = {"secout_bics_1_tech.bin", "secout_xchng_us.bin"};
  for(auto& name : names) {
    auto series = read_series_from_file(name);
    auto compressed = byte_compress_from_delta(*series);
    dump_compressed_to_file(*compressed, name + ".comp");
    auto data_ratio = 1.0*series->data_bytes() / compressed->data_bytes();
    auto total_ratio = 1.0*series->total_bytes() / compressed->total_bytes();
    LOG(name << " : data_ratio=" << data_ratio << ", total_ratio=" << total_ratio
        << ", data=" << series->data_bytes() << ", total=" << series->total_bytes() );
  }
}

int main(int argc, char** argv) {
  test_all();
  //byte_compress_real_series();
  return 0;
}

