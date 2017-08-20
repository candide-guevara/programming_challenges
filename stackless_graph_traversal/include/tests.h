#pragma once

#include "graph.h"

void test_build_nodes();
void test_graph_image_dump(uint32_t);
void test_graph_persist_to_buf(uint32_t);

void test_destructive_std_depth_first_traversal();
void test_destructive_pointer_reversal_traversal(uint32_t);
void test_destructive_pointer_back_and_forth_traversal(uint32_t);
void test_pointer_reversal_traversal(uint32_t size);

void helper_traversal_on_real_graphs(uint32_t, TraversalAlgo_t);
void helper_non_destructive_traversal_on_real_graphs(uint32_t, TraversalAlgo_t);
void helper_traversal_on_three_four_nodes(TraversalAlgo_t);
void helper_traversal_on_branches(TraversalAlgo_t);
void helper_traversal_on_single_branch(TraversalAlgo_t);
void helper_traversal_on_loop_to_itself(TraversalAlgo_t);

