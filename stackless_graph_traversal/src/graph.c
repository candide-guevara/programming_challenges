#include <graph.h>

#include <common.h>
#include <util.h>

GraphHandle build_graph_without_any_edges(uint32_t size) {
  ASSERT(size, "Cannot create an empty graph");
  GraphHandle graph;
  graph.root = calloc(size, sizeof(CountedNode));
  graph.vertex_count = size;
  ASSERT(graph.root, "Could not allocate root memory");

  for(uint32_t i=0; i<size; ++i)
    build_counted_node_in_place(graph.root + i, NULL);

  LOG_INFO("Built graph with %d nodes", graph.vertex_count);
  return graph;
}

GraphHandle build_graph_dag(uint32_t size) {
  GraphHandle graph = build_graph_without_any_edges(size);

  // Invariant :
  // graph nodes = [ RootNode, LinkedNode, LinkedNode, FreeNode, FreeNode ... ]
  //                                ^                      ^
  //                             current               next_free
  for(uint32_t current=0, next_free=1, created=0; 
      next_free < size && current < next_free; 
      ++current, created=0) {

    // we iterate on current node until we manage to link at least 1 slot to a new node
    for(uint32_t slot=0; 
        next_free < size && (created < 1 || slot < SLOT_COUNT); 
        ++slot) {

      uint32_t percent = (rand() % 100) + 1;
      if (percent > POINTER_DENSITY_PERC) continue;

      Node* predecessor = (Node*)(graph.root + current);
      Node* child       = (Node*)(graph.root + next_free);

      LOG_TRACE("Link %s -> %s", predecessor->name, child->name);
      predecessor->slots[slot % SLOT_COUNT] = child;

      created += 1;
      next_free += 1;
    }
  }
  return graph;
}

GraphHandle build_graph_dag_maybe_disconnected(uint32_t size) {
  GraphHandle graph = build_graph_without_any_edges(size);

  for(uint32_t current=0, next_free=1; next_free < size; ++current) {
    ASSERT(next_free >= current, "next_free should stay ahead of current");
    if (next_free == current) next_free += 1;

    for(uint32_t slot=0; next_free < size && slot < SLOT_COUNT; ++slot) {
      uint32_t percent = (rand() % 100) + 1;
      if (percent > POINTER_DENSITY_PERC) continue;

      Node* predecessor = (Node*)(graph.root + current);
      Node* child       = (Node*)(graph.root + next_free);

      LOG_TRACE("Link %s -> %s", predecessor->name, child->name);
      predecessor->slots[slot] = child;

      next_free += 1;
    }
  }
  return graph;
}

GraphHandle build_graph_with_undirected_cycles(uint32_t size) {
  GraphHandle graph = build_graph_dag(size);

  for(uint32_t current=1; current < size; ++current) {
    Node* node   = (Node*)(graph.root + current);
    Node** slots = node->slots;

    for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
      uint32_t percent = (rand() % 100) + 1;
      uint32_t already_there = 0;

      if (slots[slot]) continue;
      if (percent > CYCLE_DENSITY_PERC) continue;

      // we ty to link to a node behind us (unless there is already a link to it)
      Node* ancestor = (Node*)(graph.root + rand() % size);
      for(; already_there < SLOT_COUNT && slots[already_there] != ancestor; ++already_there);

      if (already_there == SLOT_COUNT) {
        LOG_TRACE("Link %s <- %s", ancestor->name, node->name);
        slots[slot] = ancestor;
      }
    }
  }
  return graph;
}

GraphHandle build_graph_amorphous(uint32_t size) {
  GraphHandle graph = build_graph_without_any_edges(size);

  for(uint32_t current=1; current < size; ++current) {
    Node* node   = (Node*)(graph.root + current);
    Node** slots = node->slots;

    for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
      uint32_t percent = (rand() % 100) + 1;
      uint32_t already_there = 0;

      if (percent > CYCLE_DENSITY_PERC) continue;

      // we ty to link to any node (unless there is already a link to it)
      Node* ancestor = (Node*)(graph.root + rand() % size);
      for(; already_there < SLOT_COUNT && slots[already_there] != ancestor; ++already_there);

      if (already_there == SLOT_COUNT) {
        LOG_TRACE("Link %s <- %s", ancestor->name, node->name);
        slots[slot] = ancestor;
      }
    }
  }
  return graph;
}

void build_graph_with_cycles_in_helper(void* state, Node* node) {
  Stack* stack = (Stack*) state;
  push(stack, node);
}

void build_graph_with_cycles_out_helper(void* state, Node* node) {
  Stack* stack = (Stack*) state;
  pop(stack); // the result of pop should equal the node argument
  if (!stack->next) return;

  for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
    uint32_t percent = (rand() % 100) + 1;
    uint32_t already_there = 0;

    if (node->slots[slot]) continue;
    if (percent > CYCLE_DENSITY_PERC) continue;

    // we ty to link to a node in the same branch (unless there is already a link to it)
    Node* ancestor = (Node*)peek(stack, rand() % stack->next);
    for(; already_there < SLOT_COUNT && node->slots[already_there] != ancestor; ++already_there);

    if (already_there == SLOT_COUNT) {
      LOG_TRACE("Link %s <- %s", ancestor->name, node->name);
      node->slots[slot] = ancestor;
    }
  }
}

GraphHandle build_graph_with_cycles(uint32_t size) {
  GraphHandle graph = build_graph_dag(size);
  Stack stack = build_stack(graph.vertex_count);
  // we are going to mutate the graph while we traverse it
  // to avoid messing with the traversal, we mutate the nodes on our way out
  two_way_depth_first_traversal((Node*)graph.root, &stack, 
      build_graph_with_cycles_in_helper, build_graph_with_cycles_out_helper);

  free_stack(&stack);
  return graph;
}

void free_graph(GraphHandle graph) {
  if (graph.root && graph.vertex_count)
    free(graph.root);
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void standard_depth_first_traversal(Node* node, void* state, void (*visitor)(void*, Node*)) {
  if (!node) return;
  LOG_TRACE("Visit : %s", node->name);
  visitor(state, node);

  for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
    Node* child = node->slots[slot];
    standard_depth_first_traversal(child, state, visitor);
  }
}

void two_way_depth_first_traversal(Node* node, void* state, 
                                   void (*in_visitor)(void*, Node*), void (*out_visitor)(void*, Node*)) {
  if (!node) return;
  LOG_TRACE("In visit : %s", node->name);
  if (in_visitor) in_visitor(state, node);

  for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
    Node* child = node->slots[slot];
    two_way_depth_first_traversal(child, state, in_visitor, out_visitor);
  }

  LOG_TRACE("Out visit : %s", node->name);
  if (out_visitor) out_visitor(state, node);
}

void pointer_reversal_traversal(Node* node, void* state, void (*visitor)(void*, Node*)) {
}

void destructive_pointer_reversal_traversal(Node* node, void* state, void (*visitor)(void*, Node*)) {
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void dump_graph_dot_format(GraphHandle graph, const char* filepath) {
  FILE* dot_file = fopen(filepath, "w");
  LOG_INFO("Dumping graph to %s", filepath);
  ASSERT(dot_file, "Failed to open file");

  fprintf(dot_file, "strict digraph {\n");
  fprintf(dot_file, "  node  [ nodesep=1.5 ];\n");
  fprintf(dot_file, "  graph [ overlap=false ];\n");
  fprintf(dot_file, "  edge  [ weight=0.5 ];\n");

  for(uint32_t i=0; i<graph.vertex_count; ++i) {
    Node* node = (Node*)(graph.root + i);
    for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
      Node* child = node->slots[slot];
      if (child)
        fprintf(dot_file, "  \"%s\" -> \"%s\"\n", node->name, child->name);
    }
  }

  fprintf(dot_file, "}\n");
  fclose(dot_file);
}

