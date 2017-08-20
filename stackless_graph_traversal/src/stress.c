#include <stress.h>

#include <stdlib.h>
#include <string.h>

#include <logger.h>
#include <common.h>
#include <util.h>
#include <graph.h>
#include <special_traversals.h>

struct rusage differential_traversal_measurement(
    GraphHandle graph, PersistedGraph graph_buf, TraversalAlgo_t traversal_algo) {
  const uint32_t repetitions = 16;
  struct rusage final_chrono;

  clear_chrono_by_id(__CHRONO_FIRST__);
  CHRONO_START(__CHRONO_FIRST__);
  for(uint32_t repeat=0; repeat<repetitions; ++repeat) {
    restore_graph_from_buffer_no_offset_adjust(graph_buf, graph);
    traversal_algo(graph.root, NULL, nop_visitor);
  }
  CHRONO_STOP(__CHRONO_FIRST__);

  clear_chrono_by_id(__CHRONO_SECOND__);
  CHRONO_START(__CHRONO_SECOND__);
  for(uint32_t repeat=0; repeat<repetitions; ++repeat) {
    restore_graph_from_buffer_no_offset_adjust(graph_buf, graph);
  }
  CHRONO_STOP(__CHRONO_SECOND__);

  diff_chrono_by_id(&final_chrono, __CHRONO_SECOND__, __CHRONO_FIRST__);
  return final_chrono;
}

uint32_t stress_evaluate_runtime_complexity_helper(
    uint32_t max_size, GraphBuilder_t graph_builder, TraversalAlgo_t traversal_algo, 
    ChronoId chrono, char* buffer, uint32_t buflen) {
  uint32_t init_buflen = buflen;
  
  for(uint32_t size=8192; size<max_size; size+=max_size/51) {
    GraphHandle graph = graph_builder(size);
    PersistedGraph graph_buf = persist_graph_to_new_buffer(graph);

    struct rusage measurement = differential_traversal_measurement(graph, graph_buf, traversal_algo);
    uint32_t written = chrono_to_csv_by_obj(
      buffer, buflen, ChronoId_to_string(chrono), &measurement);

    TEST_ASSERT(written < buflen, "Buffer too short to collect stats");
    LOG_TRACE("For %u took : %s", size, buffer);
    buffer += written;
    buflen -= written;

    free_graph(graph);
    free_persisted_graph(graph_buf);
  }

  // the number of chars written (excluding terminating '\0')
  return init_buflen - buflen;
}

void stress_profile_algo(uint32_t size, PersistedGraph graph_buf, GraphBuilder_t graph_builder, TraversalAlgo_t traversal_algo) {
  const uint32_t repetitions = 16;
  GraphHandle graph = graph_builder(size);
  persist_graph_to_old_buffer(&graph_buf, graph);

  for(uint32_t repeat=0; repeat<repetitions; ++repeat) {
    if (repeat)
      restore_graph_from_buffer_no_offset_adjust(graph_buf, graph);
    traversal_algo(graph.root, NULL, nop_visitor);
  }
  free_graph(graph);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void stress_profile_algo_on_graph_type (uint32_t size, const char* report_name, const char* algo_name, const char* graph_name) {
  TraversalAlgo_t traversal_algo = get_function_by_name(algo_name);
  if (traversal_algo == NULL) return;
  PersistedGraph graph_buf = build_persist_graph_buffer(size);

  if (is_null_or_empty(graph_name)) {
    stress_profile_algo(size, graph_buf, build_graph_dag, traversal_algo);
    stress_profile_algo(size, graph_buf, build_graph_with_undirected_cycles, traversal_algo);
    stress_profile_algo(size, graph_buf, build_graph_with_cycles, traversal_algo);
  }
  else {
    GraphBuilder_t builder_func = get_function_by_name(graph_name);
    stress_profile_algo(size, graph_buf, builder_func, traversal_algo);
  }
  free_persisted_graph(graph_buf);
}

void stress_runtime_complexity_all_algo (uint32_t size, const char* report_name, const char* algo_name, const char* graph_name) {
  char* buffer = malloc(1024*1024);
  uint32_t written, bufpos=0, buflen=1024*1024;

  FILE *report = fopen(report_name, "w");
  ASSERT(report, "Failed to open for write : %s", report_name);

  written = chrono_header_to_csv(buffer, buflen);
  bufpos += written;
  buflen -= written;

#define stress_evaluate_runtime_complexity_macro_helper(builder, algo, enum_name) \
  written = stress_evaluate_runtime_complexity_helper( \
    size, builder, algo,                               \
    enum_name, buffer + bufpos, buflen);               \
  bufpos += written;                                   \
  buflen -= written;

#define stress_evaluate_runtime_complexity_macro(algo, enum_prefix) \
  stress_evaluate_runtime_complexity_macro_helper(build_graph_dag, algo, enum_prefix ## _DAG); \
  stress_evaluate_runtime_complexity_macro_helper(build_graph_with_undirected_cycles, algo, enum_prefix ## _UCYCLE); \
  stress_evaluate_runtime_complexity_macro_helper(build_graph_with_cycles, algo, enum_prefix ## _DCYCLE)

  stress_evaluate_runtime_complexity_macro (destructive_pointer_reversal_traversal,       DESTRUCT_PTR_REVERSAL);
  stress_evaluate_runtime_complexity_macro (destructive_pointer_back_and_forth_traversal, DESTRUCT_BACK_FORTH);
  stress_evaluate_runtime_complexity_macro (pointer_reversal_traversal,                   PTR_REVERSAL);
  stress_evaluate_runtime_complexity_macro (destructive_std_depth_first_traversal,        STD_DEPTH_FIRST);

  written = fwrite(buffer, sizeof(char), bufpos, report);
  ASSERT(written == bufpos, "Failed to write report : %s", report_name);
  free(buffer);
  fclose(report);
}

