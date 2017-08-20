#include <string.h>

#include <logger.h>
#include <common.h>
#include <util.h>

#include <special_traversals.h>
#include <tests.h>
#include <stress.h>

int launch_all_tests(uint32_t, const char*);
int launch_stress_by_name(uint32_t, const char*, int, const char**);
int enumerate_available_algos();

enum RETURN_CODES {
  RETURN_OK,
  RETURN_BAD_GRAPH_SIZE,
  RETURN_UNKNOWN_ACTION,
  RETURN_INKNOWN_STRESS_OP,
};

#pragma GCC diagnostic ignored "-Wunused-variable"
int main (int argc, const char **argv) {
  const char *op_name = argc > 1 ? argv[1] : "";
  uint32_t graph_size = 128;
  if (argc > 2) graph_size = atoi_with_base(argv[2]);

  if (graph_size > 1024*1024*1024) {
    LOG_ERROR("graph_size is too big (%u), aborting ...", graph_size);
    return RETURN_BAD_GRAPH_SIZE;
  }

  if (*op_name == '\0' || strcmp(op_name, "test") == 0)
    return launch_all_tests(graph_size, op_name);
  else if (strncmp(op_name, "stress", 6) == 0)
    return launch_stress_by_name(graph_size, op_name, argc, argv);
  else if (strcmp(op_name, "list_algo") == 0)
    return enumerate_available_algos();
  else {
    LOG_ERROR("No action found for '%s' and size %u", op_name, graph_size);
    return RETURN_UNKNOWN_ACTION;
  }
}
#pragma GCC diagnostic pop

int launch_all_tests(uint32_t graph_size, const char* test_name) {
  test_build_nodes();
  test_graph_image_dump(graph_size);
  test_destructive_std_depth_first_traversal();
  test_destructive_pointer_reversal_traversal(graph_size);
  test_destructive_pointer_back_and_forth_traversal(graph_size);
  test_pointer_reversal_traversal(graph_size);
  test_graph_persist_to_buf(graph_size);

  LOG_WARN("All tests done !!");
  return RETURN_OK;
}

// Examples : 
// ./project stress_runtime_complexity_all_algo 128K report.log "" ""
// ./project stress_profile_algo_on_graph_type 128K "" destructive_std_depth_first_traversal ""
// ./project stress_profile_algo_on_graph_type 128K "" report_name pointer_reversal_traversal build_graph_dag
int launch_stress_by_name(uint32_t graph_size, const char* stress_name, int argc, const char **argv) {
  const char *report_name = argc > 3 ? argv[3] : "";
  const char *algo_name   = argc > 4 ? argv[4] : "";
  const char *graph_name  = argc > 5 ? argv[5] : "";

  StressOperation_t stress_operation = get_function_by_name(stress_name);
  if (!stress_operation) return RETURN_INKNOWN_STRESS_OP;

  stress_operation(graph_size, report_name, algo_name, graph_name);
  LOG_WARN("All stress operations done !!");
  return RETURN_OK;
}

int enumerate_available_algos() {
  printf("%s\n%s\n%s\n%s\n",
    "destructive_std_depth_first_traversal",
    "destructive_pointer_reversal_traversal",
    "destructive_pointer_back_and_forth_traversal",
    "pointer_reversal_traversal"
  );
  return RETURN_OK;
}

