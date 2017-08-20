#pragma once

#include <stdint.h>

#include "graph.h"
#include "common.h"

typedef struct Stack Stack;

struct Stack {
  void ** items;
  size_t capacity, next;
};

Stack build_stack(size_t capacity);
void free_stack(Stack* stack);
void push(Stack* stack, void* item);
void* pop(Stack* stack);
void* peek(Stack* stack, size_t index);

////////////////////////////////////////////////////////////////////////////

typedef struct SillyVector SillyVector;

struct SillyVector {
  void **items;
  size_t capacity, size;
};

SillyVector build_vector(size_t capacity);
void free_vector(SillyVector* vector);
void* vector_append(SillyVector* vector, void* item);
uint32_t vector_contains(SillyVector* vector, void* item);
uint32_t vector_size(SillyVector* vector);
uint32_t vector_capacity(SillyVector* vector);

////////////////////////////////////////////////////////////////////////////

union PointerFlag {
  void* pointer;
  size_t flag;
};
typedef union PointerFlag PointerFlag;

STATIC_ASSERT((sizeof(Node) >= 8), cannot_fit_all_flags);
const static size_t SET_BACK_FLAG = 1;
const static size_t DEL_BACK_FLAG = ~0 ^ 1;
const static size_t SET_TRAVERSE_FLAG = 2;
const static size_t DEL_TRAVERSE_FLAG = ~0 ^ 2;
const static size_t SET_VISIT_FLAG = 4;
const static size_t DEL_VISIT_FLAG = ~0 ^ 4;
const static size_t SET_ALL_FLAG = 7; // DEL_BACK_FLAG | DEL_TRAVERSE_FLAG | DEL_VISIT_FLAG;
const static size_t DEL_ALL_FLAG = ~0 ^ 7; // DEL_BACK_FLAG & DEL_TRAVERSE_FLAG & DEL_VISIT_FLAG;

#pragma GCC diagnostic ignored "-Wunused-function"
static void* set_flags_on_edge(void* edge, size_t flags) {
  PointerFlag pt_flag;
  pt_flag.pointer = edge;
  pt_flag.flag |= flags;
  return pt_flag.pointer;
}

static size_t get_flags_on_edge(void* edge, size_t flags) {
  PointerFlag pt_flag;
  pt_flag.pointer = edge;
  pt_flag.flag &= flags;
  return pt_flag.flag;
}

static void* toggle_flags_on_edge(void* edge, size_t flags) {
  PointerFlag pt_flag;
  pt_flag.pointer = edge;
  pt_flag.flag ^= flags;
  return pt_flag.pointer;
}

static void* clear_flags_on_edge(void* edge, size_t flags) {
  return (void*)get_flags_on_edge(edge, ~flags);
}

static void* to_backwards_edge(void* edge) {
  return set_flags_on_edge(edge, SET_BACK_FLAG);
}

static size_t is_backwards(void* edge) {
  return get_flags_on_edge(edge, SET_BACK_FLAG);
}

static void* follow_edge(void* edge) {
  return (void*)get_flags_on_edge(edge, DEL_ALL_FLAG);
}
#pragma GCC diagnostic pop

uint32_t is_leaf_node(Node *node);
uint32_t is_leaf_node_ignore_back(Node *node);
void fprintf_node_dot_format(FILE* dot_file, Node *node, const char* color);
void fprintf_edge_dot_format(FILE* dot_file, Node *node, Node *child, const char* color);

////////////////////////////////////////////////////////////////////////////

GraphHandle build_graph_diamond();
GraphHandle build_graph_triangle();
GraphHandle build_graph_unbalanced_branches();
GraphHandle build_graph_branch_with_fanout();
GraphHandle construct_and_dump_to_file(GraphHandle (*)(uint32_t), uint32_t, const char*);

////////////////////////////////////////////////////////////////////////////

typedef struct CountState CountState;

struct CountState {
  uint32_t counter;
  // Points to the first node visited out of order
  Node *first_out_of_order;
  Node *last_visit_node;
};

VisitorState* new_count_state();
void reset_count_state(VisitorState* visit_state, GraphHandle new_graph);
void free_count_state(VisitorState *visit_state);

void count_visitor (VisitorState* visit_state, Node* node);
void monotonic_count_visitor (VisitorState* visit_state, Node* node);
void nop_visitor (VisitorState* visit_state, Node* node);

////////////////////////////////////////////////////////////////////////////

// Bases accepted : K=1024, M=1024**2, G=1024**3
int64_t atoi_with_base(const char *str_int);

