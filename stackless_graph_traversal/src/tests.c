#include <common.h>
#include <util.h>
#include <graph.h>
#include <special_traversals.h>
#include <tests.h>

void test_build_nodes () {
  #pragma GCC diagnostic ignored "-Wunused-variable"
  for(int i=0; i<5; ++i) {
    Node n2 = build_node(i);
    LOG_INFO("Created %s", print_node(&n2));
  }
  #pragma GCC diagnostic pop
}

void test_graph_image_dump(uint32_t graph_size) {
  GraphHandle graphs[5];
  graphs[0] = construct_and_dump_to_file(build_graph_dag, graph_size, "test_build_graph_dag");
  graphs[1] = construct_and_dump_to_file(build_graph_dag_maybe_disconnected, graph_size, "test_build_graph_dag_maybe_disconnected");
  graphs[2] = construct_and_dump_to_file(build_graph_with_cycles, graph_size, "test_build_graph_with_cycles");
  graphs[3] = construct_and_dump_to_file(build_graph_with_undirected_cycles, graph_size, "test_build_graph_with_undirected_cycles");
  graphs[4] = construct_and_dump_to_file(build_graph_amorphous, graph_size, "test_build_graph_amorphous");

  for(int i=0; i<sizeof(graphs)/sizeof(GraphHandle); ++i)
    free_graph(graphs[i]);
}

// We use a macro to preserve line number in case of errors
#define ASSERT_VISIT_COUNT(traversal_algo, visit_state, strict) do {                                      \
  CountState* user_state = (CountState*)visit_state->user_state;                                          \
  uint32_t condition = user_state->counter == visit_state->graph.vertex_count;                            \
  if (!(strict))                                                                                          \
    condition = user_state->counter <= visit_state->graph.vertex_count;                                   \
  TEST_ASSERT(condition, "Visited the wrong number of nodes : %u", user_state->counter);                  \
  if (traversal_algo == destructive_pointer_back_and_forth_traversal) {                                   \
    condition = user_state->first_out_of_order == NULL;                                                   \
    TEST_ASSERT(condition, "Visited nodes in the wrong order at : %p", user_state->first_out_of_order);   \
  }                                                                                                       \
} while(0)

void helper_traversal_on_real_graphs(uint32_t graph_size, TraversalAlgo_t traversal_algo) {
  VisitorState* visit_state = new_count_state();
  GraphHandle graphs[5];
  Node* start_node = NULL;

  graphs[0] = build_graph_dag(graph_size);
  reset_count_state(visit_state, graphs[0]);
  traversal_algo(graphs[0].root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);

  graphs[1] = build_graph_dag_maybe_disconnected(graph_size);
  reset_count_state(visit_state, graphs[1]);
  start_node = graphs[1].root + rand() % graph_size;
  traversal_algo(start_node, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, false);

  graphs[2] = build_graph_with_cycles(graph_size);
  reset_count_state(visit_state, graphs[2]);
  traversal_algo(graphs[2].root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);

  graphs[3] = build_graph_with_undirected_cycles(graph_size);
  reset_count_state(visit_state, graphs[3]);
  traversal_algo(graphs[3].root, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);

  graphs[4] = build_graph_amorphous(graph_size);
  reset_count_state(visit_state, graphs[4]);
  start_node = graphs[4].root + rand() % graph_size;
  traversal_algo(start_node, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, false);

  for(int i=0; i<sizeof(graphs)/sizeof(GraphHandle); ++i)
    free_graph(graphs[i]);
  free_count_state(visit_state);
}

void helper_non_destructive_traversal_on_real_graphs(uint32_t graph_size, TraversalAlgo_t traversal_algo) {
  VisitorState* visit_state = new_count_state();
  GraphHandle graphs[3];

  graphs[0] = build_graph_dag(graph_size);
  for(uint32_t i=0; i<3; ++i) {
    reset_count_state(visit_state, graphs[0]);
    traversal_algo(graphs[0].root, visit_state, monotonic_count_visitor);
    ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  }

  graphs[1] = build_graph_with_cycles(graph_size);
  for(uint32_t i=0; i<3; ++i) {
    reset_count_state(visit_state, graphs[1]);
    traversal_algo(graphs[1].root, visit_state, monotonic_count_visitor);
    ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  }

  graphs[2] = build_graph_with_undirected_cycles(graph_size);
  for(uint32_t i=0; i<3; ++i) {
    reset_count_state(visit_state, graphs[2]);
    traversal_algo(graphs[2].root, visit_state, count_visitor);
    ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  }

  for(int i=0; i<sizeof(graphs)/sizeof(GraphHandle); ++i)
    free_graph(graphs[i]);
  free_count_state(visit_state);
}

void helper_traversal_on_three_four_nodes(TraversalAlgo_t traversal_algo) {
  VisitorState* visit_state = new_count_state();

  GraphHandle graph = build_graph_triangle();
  reset_count_state(visit_state, graph);
  LOG_INFO("Validating triangle graph");
  traversal_algo(graph.root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_diamond();
  reset_count_state(visit_state, graph);
  LOG_INFO("Validating diamond graph");
  traversal_algo(graph.root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_diamond();
  reset_count_state(visit_state, graph);
  graph.root[1].slots[1] = graph.root + 0;
  graph.root[2].slots[1] = graph.root + 0;
  graph.root[3].slots[0] = graph.root + 1;
  graph.root[3].slots[1] = graph.root + 2;
  LOG_INFO("Validating double linked diamond graph");
  traversal_algo(graph.root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_diamond();
  reset_count_state(visit_state, graph);
  graph.root[0].slots[2] = graph.root + 3;
  graph.root[1].slots[1] = graph.root + 0;
  graph.root[1].slots[2] = graph.root + 2;
  graph.root[2].slots[1] = graph.root + 0;
  graph.root[2].slots[2] = graph.root + 1;
  graph.root[3].slots[0] = graph.root + 0;
  graph.root[3].slots[1] = graph.root + 1;
  graph.root[3].slots[2] = graph.root + 2;
  LOG_INFO("Validating triple linked diamond graph");
  traversal_algo(graph.root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  free_count_state(visit_state);
}

void helper_traversal_on_branches(TraversalAlgo_t traversal_algo) {
  VisitorState* visit_state = new_count_state();

  GraphHandle graph = build_graph_unbalanced_branches();
  reset_count_state(visit_state, graph);
  LOG_INFO("Validating unbalanced branches graph");
  traversal_algo(graph.root, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_unbalanced_branches();
  reset_count_state(visit_state, graph);
  graph.root[6].slots[0] = graph.root + 1;
  LOG_INFO("Validating unbalanced branches with cycle undirected graph");
  traversal_algo(graph.root, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_unbalanced_branches();
  reset_count_state(visit_state, graph);
  graph.root[6].slots[0] = graph.root + 1;
  graph.root[4].slots[0] = graph.root + 6;
  LOG_INFO("Validating unbalanced branches with cycle directed graph");
  traversal_algo(graph.root, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  free_count_state(visit_state);
}

void helper_traversal_on_single_branch(TraversalAlgo_t traversal_algo) {
  VisitorState* visit_state = new_count_state();

  GraphHandle graph = build_graph_single_branch(6);
  reset_count_state(visit_state, graph);
  LOG_INFO("Validating single branch graph");
  traversal_algo(graph.root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_branch_with_fanout();
  reset_count_state(visit_state, graph);
  LOG_INFO("Validating single branch with fanout graph");
  traversal_algo(graph.root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_single_branch(6);
  reset_count_state(visit_state, graph);
  graph.root[5].slots[0] = graph.root + 1;
  graph.root[4].slots[1] = graph.root + 1;
  graph.root[3].slots[1] = graph.root;
  LOG_INFO("Validating single branch with backward cycles");
  traversal_algo(graph.root, visit_state, monotonic_count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  free_count_state(visit_state);
}

void helper_traversal_on_loop_to_itself(TraversalAlgo_t traversal_algo) {
  VisitorState* visit_state = new_count_state();

  GraphHandle graph = build_graph_single_branch(1);
  reset_count_state(visit_state, graph);
  graph.root[0].slots[0] = graph.root;
  graph.root[0].slots[1] = graph.root;
  LOG_INFO("Validating single node with loops to itself");
  traversal_algo(graph.root, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  graph = build_graph_single_branch(5);
  reset_count_state(visit_state, graph);
  graph.root[2].slots[1] = graph.root + 2;
  graph.root[4].slots[0] = graph.root + 4;
  graph.root[4].slots[1] = graph.root + 4;
  LOG_INFO("Validating single branch with looping nodes");
  traversal_algo(graph.root, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(traversal_algo, visit_state, true);
  free_graph(graph);

  free_count_state(visit_state);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void test_destructive_pointer_reversal_traversal(uint32_t size) {
  helper_traversal_on_three_four_nodes(destructive_pointer_reversal_traversal);
  helper_traversal_on_branches(destructive_pointer_reversal_traversal);
  helper_traversal_on_single_branch(destructive_pointer_reversal_traversal);
  helper_traversal_on_loop_to_itself(destructive_pointer_reversal_traversal);
  helper_traversal_on_real_graphs(size, destructive_pointer_reversal_traversal);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void test_destructive_pointer_back_and_forth_traversal(uint32_t size) {
  helper_traversal_on_three_four_nodes(destructive_pointer_back_and_forth_traversal);
  helper_traversal_on_branches(destructive_pointer_back_and_forth_traversal);
  helper_traversal_on_single_branch(destructive_pointer_back_and_forth_traversal);
  helper_traversal_on_loop_to_itself(destructive_pointer_back_and_forth_traversal);
  helper_traversal_on_real_graphs(size, destructive_pointer_back_and_forth_traversal);
}

void test_pointer_reversal_traversal(uint32_t size) {
  helper_traversal_on_three_four_nodes(pointer_reversal_traversal);
  helper_traversal_on_branches(pointer_reversal_traversal);
  helper_traversal_on_single_branch(pointer_reversal_traversal);
  helper_traversal_on_loop_to_itself(pointer_reversal_traversal);
  helper_traversal_on_real_graphs(size, pointer_reversal_traversal);
  helper_non_destructive_traversal_on_real_graphs(size, pointer_reversal_traversal);

  /*VisitorState* visit_state = new_count_state();
  GraphHandle graph = build_graph_with_cycles(size);
  reset_count_state(visit_state, graph);
  pointer_reversal_traversal(graph.root, visit_state, count_visitor);
  ASSERT_VISIT_COUNT(pointer_reversal_traversal, visit_state, true);
  free_graph(graph);
  free_count_state(visit_state);*/
}

