#pragma once

#include "node.h"

typedef struct GraphHandle GraphHandle;
typedef struct PersistedGraph PersistedGraph;
typedef struct VisitorState VisitorState;

struct GraphHandle {
  Node* root;
  uint32_t vertex_count;
};

struct PersistedGraph {
  Node* root;
  uint32_t vertex_count;
  uint32_t slot_count;
  union {
    uint64_t as_int;
    void* as_ptr;
  } offset;
};

struct VisitorState {
  void* user_state;
  GraphHandle graph;
};

typedef void (*Visitor_t)(VisitorState*, Node*);
typedef void (*TraversalAlgo_t)(Node*, VisitorState*, Visitor_t);
typedef GraphHandle (*GraphBuilder_t)(uint32_t);

void free_graph(GraphHandle graph);
void free_persisted_graph(PersistedGraph graph);

GraphHandle build_graph_without_any_edges(uint32_t size);
GraphHandle build_graph_dag(uint32_t size);
GraphHandle build_graph_dag_maybe_disconnected(uint32_t size);
GraphHandle build_graph_with_cycles(uint32_t size);
GraphHandle build_graph_with_undirected_cycles(uint32_t size);
GraphHandle build_graph_amorphous(uint32_t size);

GraphHandle build_graph_single_branch(uint32_t size);

void standard_depth_first_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor);
void two_way_depth_first_traversal(Node* node, VisitorState* visit_state, 
                                    Visitor_t in_visitor, Visitor_t out_visitor);

void dump_graph_dot_format(GraphHandle, const char*);
PersistedGraph build_persist_graph_buffer(uint32_t);
PersistedGraph persist_graph_to_new_buffer(GraphHandle);
void persist_graph_to_old_buffer(PersistedGraph*, GraphHandle);
GraphHandle restore_graph_from_buffer(PersistedGraph);
void restore_graph_from_buffer_no_offset_adjust(PersistedGraph, GraphHandle);

