#pragma once

#include "node.h"

typedef struct GraphHandle GraphHandle;
typedef struct VisitorState VisitorState;

typedef void (*Visitor_t)(VisitorState*, Node*);
typedef void (*TraversalAlgo_t)(Node*, VisitorState*, Visitor_t);

struct GraphHandle {
  Node* root;
  uint32_t vertex_count;
};

struct VisitorState {
  void* user_state;
  GraphHandle graph;
};

void free_graph(GraphHandle graph);

GraphHandle build_graph_without_any_edges(uint32_t size);
GraphHandle build_graph_dag(uint32_t size);
GraphHandle build_graph_dag_maybe_disconnected(uint32_t size);
GraphHandle build_graph_with_cycles(uint32_t size);
GraphHandle build_graph_with_undirected_cycles(uint32_t size);
GraphHandle build_graph_amorphous(uint32_t size);

GraphHandle build_graph_single_branch(uint32_t size);

// Infinite recursion if cycles
void standard_depth_first_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor);
void two_way_depth_first_traversal(Node* node, VisitorState* visit_state, 
                                    Visitor_t in_visitor, Visitor_t out_visitor);

void dump_graph_dot_format(GraphHandle graph, const char* filepath);

