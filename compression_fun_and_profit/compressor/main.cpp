#include <series_io.hpp>
#include <algorithm>

void test_read_prob_dstrb_from_file() {
  auto dstrb = read_prob_dstrb_from_file("prob_dstrb_bics_1_tech.txt");
  TEST_ASSERT(dstrb->size() >= ALPHA_LEN * 2); 
  TEST_ASSERT(std::is_sorted(RANGE(*dstrb), [](auto a, auto b) { return cumsum(a) < cumsum(b); }));
  TEST_ASSERT(std::all_of(RANGE(*dstrb), [](auto t) { return weight(t) < MAX_PROB; }));
  TEST_ASSERT(cumsum(dstrb->back()) == MAX_PROB);
  //LOG("\n" << prod_to_string(*dstrb));
}

int main(int argc, char** argv) {
  test_read_prob_dstrb_from_file();
  return 0;
}

