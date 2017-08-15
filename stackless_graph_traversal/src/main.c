#include <common.h>
#include <tests.h>
#include <util.h>
#include <special_traversals.h>

int main (void) {
  LOG_WARN("Starting ...");
  #pragma GCC diagnostic ignored "-Wunused-variable"
  uint32_t graph_size = 64;
  #pragma GCC diagnostic pop

  test_build_nodes();
  test_graph_image_dump(graph_size);
  test_destructive_pointer_reversal_traversal(graph_size);
  test_destructive_pointer_back_and_forth_traversal(graph_size);
  test_pointer_reversal_traversal(graph_size);

  LOG_WARN("All done !!");
  return 0;
}

