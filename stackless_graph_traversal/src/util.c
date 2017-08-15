#include <util.h>

#include <common.h>

Stack build_stack(size_t capacity) {
  Stack stack;
  stack.next = 0;
  stack.capacity = capacity;
  stack.items = calloc(capacity, sizeof(void*));
  ASSERT(stack.items, "Failed to allocate mem for stack");
  return stack;
}

void free_stack(Stack* stack) {
  if (stack->items && stack->capacity)
    free(stack->items);
}

void push(Stack* stack, void* item) {
  ASSERT(stack->next < stack->capacity, "Stack overflow");
  stack->items[stack->next] = item;
  stack->next += 1;
}

void* pop(Stack* stack) {
  ASSERT(stack->next > 0, "Stack underflow");
  stack->next -= 1;
  return stack->items[stack->next];
}

void* peek(Stack* stack, size_t index) {
  ASSERT(index < stack->next, "Invalid stack location");
  return stack->items[index];
}

////////////////////////////////////////////////////////////////////////////

uint32_t is_leaf_node(Node *node) {
  uint32_t is_leaf = 0;
  for(; is_leaf < SLOT_COUNT 
      && follow_edge(node->slots[is_leaf]) == NULL; 
      ++is_leaf);
  return is_leaf == SLOT_COUNT;
}

uint32_t is_leaf_node_ignore_back(Node *node) {
  uint32_t is_leaf = 0;
  for(; is_leaf < SLOT_COUNT 
      && (node->slots[is_leaf] == NULL || is_backwards(node->slots[is_leaf])); 
      ++is_leaf);
  return is_leaf == SLOT_COUNT;
}

void fprintf_node_dot_format(FILE* dot_file, Node *node, const char* color) {
  if (!color) color = "white";
  fprintf(dot_file, "  \"%s{%u}\" [style=\"filled\"; fillcolor=\"%s\"]\n", 
          node->name, node->count, color);
}

void fprintf_edge_dot_format(FILE* dot_file, Node *node, Node *child, const char* color) {
  const char* child_name = "NULL";
  uint32_t child_count = 0;
  if (child) {
    child_name = child->name;
    child_count = child->count;
  }
  if (!color) color = "black";
  fprintf(dot_file, "  \"%s{%u}\" -> \"%s{%u}\" [color=\"%s\"]\n", 
          node->name, node->count, child_name, child_count, color);
}

////////////////////////////////////////////////////////////////////////////

VisitorState* new_count_state() {
  GraphHandle empty_graph = {0};
  CountState* user_state = malloc(sizeof(CountState));
  VisitorState* visit_state = malloc(sizeof(VisitorState));
  visit_state->user_state = user_state;
  reset_count_state(visit_state, empty_graph);
  return visit_state;
}

void free_count_state(VisitorState *visit_state) {
  if (visit_state) {
    if (visit_state->user_state)
      free(visit_state->user_state);
    free(visit_state);
  }
}

void reset_count_state(VisitorState* visit_state, GraphHandle new_graph) {
  CountState* user_state = (CountState*)visit_state->user_state;
  user_state->counter = 0;
  user_state->first_out_of_order = NULL;
  user_state->last_visit_node = NULL;
  visit_state->graph = new_graph;
}

void monotonic_count_visitor (VisitorState* visit_state, Node* node) {
  CountState* user_state = (CountState*)visit_state->user_state;
  if (user_state->first_out_of_order == NULL && node < user_state->last_visit_node)
    user_state->first_out_of_order = node;
  user_state->counter += 1;
  user_state->last_visit_node = node;
  LOG_TRACE(" - %d, %p, %s", user_state->counter, node, print_node(node));
}

void count_visitor (VisitorState* visit_state, Node* node) {
  CountState* user_state = (CountState*)visit_state->user_state;
  user_state->counter += 1;
  LOG_TRACE(" - %d, %p, %s", user_state->counter, node, print_node(node));
}

void nop_visitor (VisitorState* visit_state, Node* node) {}

////////////////////////////////////////////////////////////////////////////

GraphHandle construct_and_dump_to_file(GraphHandle (*constructor)(uint32_t), uint32_t graph_size, const char* filename) {
  char filepath[64];
  snprintf(filepath, sizeof(filepath), "%s.dot", filename);

  LOG_INFO("Building graph %s", filename);
  GraphHandle graph = constructor(graph_size);
  dump_graph_dot_format(graph, filepath);
  return graph;
}

STATIC_ASSERT((SLOT_COUNT > 1), too_few_slots);

// @breadth_order : The other of breadth first visit is the order of node allocation
GraphHandle build_graph_triangle() {
  GraphHandle graph = build_graph_without_any_edges(3);
  graph.root[0].slots[0] = graph.root + 1;
  graph.root[0].slots[1] = graph.root + 2;
  return graph;
}

// @breadth_order : The other of breadth first visit is the order of node allocation
GraphHandle build_graph_diamond() {
  GraphHandle graph = build_graph_without_any_edges(4);
  graph.root[0].slots[0] = graph.root + 1;
  graph.root[0].slots[1] = graph.root + 2;
  graph.root[1].slots[0] = graph.root + 3;
  graph.root[2].slots[0] = graph.root + 3;
  return graph;
}

GraphHandle build_graph_unbalanced_branches() {
  GraphHandle graph = build_graph_without_any_edges(7);
  graph.root[0].slots[0] = graph.root + 1;
  graph.root[1].slots[0] = graph.root + 2;
  graph.root[2].slots[0] = graph.root + 3;
  graph.root[3].slots[0] = graph.root + 4;

  graph.root[0].slots[1] = graph.root + 5;
  graph.root[5].slots[0] = graph.root + 6;
  return graph;
}

// @breadth_order : The other of breadth first visit is the order of node allocation
GraphHandle build_graph_branch_with_fanout() {
  GraphHandle graph = build_graph_without_any_edges(3 + SLOT_COUNT);
  graph.root[0].slots[0] = graph.root + 1;
  graph.root[1].slots[0] = graph.root + 2;
  for(uint32_t i=0; i<SLOT_COUNT; ++i)
    graph.root[2].slots[i] = graph.root + 3 + i;
  return graph;
}

