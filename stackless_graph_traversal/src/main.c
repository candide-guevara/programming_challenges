#include <common.h>
#include <graph.h>

void test_build_nodes();
void construct_and_dump_to_file(GraphHandle (*)(uint32_t), uint32_t, const char*);

int main (void) {
  LOG_WARN("Starting ...");
  uint32_t graph_size = 64;

  // test_build_nodes();
  construct_and_dump_to_file(build_graph_dag, graph_size, "test_build_graph_dag");
  construct_and_dump_to_file(build_graph_dag_maybe_disconnected, graph_size, "test_build_graph_dag_maybe_disconnected");
  construct_and_dump_to_file(build_graph_with_cycles, graph_size, "test_build_graph_with_cycles");
  construct_and_dump_to_file(build_graph_with_undirected_cycles, graph_size, "test_build_graph_with_undirected_cycles");
  construct_and_dump_to_file(build_graph_amorphous, graph_size, "test_build_graph_amorphous");

  LOG_INFO("All done !!");
  return 0;
}

void test_build_nodes () {
  CountedNode n1 = build_counted_node("coucou");
  LOG_INFO("Created %s", print_counted_node(&n1));

  for(int i=0; i<5; ++i) {
    Node n2 = build_node(NULL);
    LOG_INFO("Created %s", print_node(&n2));
  }
  for(int i=0; i<5; ++i) {
    CountedNode n3 = build_counted_node(NULL);
    LOG_INFO("Created %s", print_counted_node(&n3));
  }
}

void construct_and_dump_to_file(GraphHandle (*constructor)(uint32_t), uint32_t graph_size, const char* filename) {
  char filepath[64];
  snprintf(filepath, sizeof(filepath), "%s.dot", filename);

  LOG_INFO("Building graph %s", filename);
  GraphHandle graph = constructor(graph_size);
  dump_graph_dot_format(graph, filepath);
  free_graph(graph);
}

