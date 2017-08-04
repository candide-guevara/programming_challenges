#pragma once

#include "node.h"

typedef struct GraphHandle GraphHandle;

struct GraphHandle {
  CountedNode* root;
  uint32_t vertex_count;
};

GraphHandle build_graph_without_any_edges(uint32_t size);
GraphHandle build_graph_dag(uint32_t size);
GraphHandle build_graph_dag_maybe_disconnected(uint32_t size);
GraphHandle build_graph_with_cycles(uint32_t size);
GraphHandle build_graph_with_undirected_cycles(uint32_t size);
GraphHandle build_graph_amorphous(uint32_t size);
void free_graph(GraphHandle graph);

// Infinite recursion if cycles
void standard_depth_first_traversal(Node* node, void* state, void (*visitor)(void*, Node*));
void two_way_depth_first_traversal(Node* node, void* state, 
                                   void (*in_visitor)(void*, Node*), void (*out_visitor)(void*, Node*));
void pointer_reversal_traversal(Node* node, void* state, void (*visitor)(void*, Node*));
void destructive_pointer_reversal_traversal(Node* node, void* state, void (*visitor)(void*, Node*));

void dump_graph_dot_format(GraphHandle graph, const char* filepath);

