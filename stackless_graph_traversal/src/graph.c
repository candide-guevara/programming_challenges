#include <graph.h>

#include <stdlib.h>
#include <string.h>

#include <logger.h>
#include <common.h>
#include <util.h>

GraphHandle build_graph_without_any_edges(uint32_t size) {
  ASSERT(size, "Cannot create an empty graph");
  GraphHandle graph;
  graph.root = calloc(size, sizeof(Node));
  graph.vertex_count = size;
  ASSERT(graph.root, "Could not allocate root memory");

  for(uint32_t i=0; i<size; ++i)
    build_node_in_place(graph.root + i, i);

  LOG_INFO("Built graph with %d nodes", graph.vertex_count);
  return graph;
}

// @breadth_order : The other of breadth first visit is the order of node allocation
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

void build_graph_with_cycles_in_helper(VisitorState* state, Node* node) {
  Stack* stack = (Stack*) state->user_state;
  push(stack, node);
}

void build_graph_with_cycles_out_helper(VisitorState* state, Node* node) {
  Stack* stack = (Stack*) state->user_state;
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

// @breadth_order : The other of breadth first visit is the order of node allocation
GraphHandle build_graph_with_cycles(uint32_t size) {
  GraphHandle graph = build_graph_dag(size);
  Stack stack = build_stack(graph.vertex_count);
  VisitorState visit_state = { .user_state = &stack };

  // we are going to mutate the graph while we traverse it
  // to avoid messing with the traversal, we mutate the nodes on our way out
  two_way_depth_first_traversal((Node*)graph.root, &visit_state, 
      build_graph_with_cycles_in_helper, build_graph_with_cycles_out_helper);

  free_stack(&stack);
  return graph;
}

void free_graph(GraphHandle graph) {
  if (graph.root && graph.vertex_count)
    free(graph.root);
}

void free_persisted_graph(PersistedGraph graph) {
  if (graph.root && graph.vertex_count)
    free(graph.root);
}

////////////////////////////////////////////////////////////////////////////////////////////////////

// @breadth_order : The other of breadth first visit is the order of node allocation
// @depth_order : The other of depth first visit is the order of node allocation
GraphHandle build_graph_single_branch(uint32_t size) {
  GraphHandle graph = build_graph_without_any_edges(size);
  for(uint32_t i=0; i<size-1; ++i)
    graph.root[i].slots[0] = graph.root + i + 1;
  return graph;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void standard_depth_first_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor) {
  if (!node) return;
  LOG_TRACE("Visit : %s", node->name);
  visitor(visit_state, node);

  for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
    Node* child = node->slots[slot];
    standard_depth_first_traversal(child, visit_state, visitor);
  }
}

void two_way_depth_first_traversal(Node* node, VisitorState* visit_state, 
                                   Visitor_t in_visitor, Visitor_t out_visitor) {
  if (!node) return;
  LOG_TRACE("In visit : %s", node->name);
  if (in_visitor) in_visitor(visit_state, node);

  for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
    Node* child = node->slots[slot];
    two_way_depth_first_traversal(child, visit_state, in_visitor, out_visitor);
  }

  LOG_TRACE("Out visit : %s", node->name);
  if (out_visitor) out_visitor(visit_state, node);
}

////////////////////////////////////////////////////////////////////////////////////////////////////

PersistedGraph build_persist_graph_buffer(uint32_t capacity) {
  PersistedGraph graph_buf = {0};
  graph_buf.root = calloc(capacity, sizeof(Node));
  ASSERT(graph_buf.root, "Failed graph buffer allocation");
  graph_buf.vertex_count = capacity;
  return graph_buf;
}

PersistedGraph persist_graph_to_new_buffer(GraphHandle graph) {
  PersistedGraph graph_buf = build_persist_graph_buffer(graph.vertex_count);
  persist_graph_to_old_buffer(&graph_buf, graph);
  return graph_buf;
}

void persist_graph_to_old_buffer(PersistedGraph *graph_buf, GraphHandle graph) {
  ASSERT(graph.vertex_count <= graph_buf->vertex_count, "Cannot persist graph, not enough space");
  memcpy(graph_buf->root, graph.root, graph.vertex_count * sizeof(Node));
  graph_buf->vertex_count = graph.vertex_count;
  graph_buf->offset.as_ptr = graph.root;
  graph_buf->slot_count= SLOT_COUNT;
}

GraphHandle restore_graph_from_buffer(PersistedGraph graph_buf) {
  GraphHandle new_graph = {0};
  new_graph.vertex_count = graph_buf.vertex_count;
  new_graph.root = calloc(new_graph.vertex_count, sizeof(Node));
  
  restore_graph_from_buffer_no_offset_adjust(graph_buf, new_graph);

  // now we need to adjust the edge pointers to the new offset
  size_t adjust = (size_t)new_graph.root - graph_buf.offset.as_int;
  for (uint32_t i=0; i<new_graph.vertex_count; ++i)
    for (uint32_t j=0; j<SLOT_COUNT; ++j) {
      size_t edge = (size_t)(new_graph.root[i].slots[j]);
      if (follow_edge((void*)edge) == 0) continue;
      edge += adjust;
      //LOG_TRACE("Move %p -> %p", new_graph.root[i].slots[j], (void*)edge);
      new_graph.root[i].slots[j] = (void*)edge;
    }
  return new_graph;
}

void restore_graph_from_buffer_no_offset_adjust(PersistedGraph graph_buf, GraphHandle graph) {
  LOG_TRACE("Restoring graph with %d nodes into %p", graph_buf.vertex_count, graph.root);
  ASSERT(graph.vertex_count >= graph_buf.vertex_count, "Cannot restore graph, not enough space");
  ASSERT(SLOT_COUNT >= graph_buf.slot_count, "Cannot restore graph, incompatible node type");
  memcpy(graph.root, graph_buf.root, graph_buf.vertex_count * sizeof(Node));
  graph.vertex_count = graph_buf.vertex_count;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void dump_graph_dot_format(GraphHandle graph, const char* filepath) {
  FILE* dot_file = fopen(filepath, "w");
  LOG_INFO("Dumping graph to %s", filepath);
  ASSERT(dot_file, "Failed to open file");

  fprintf(dot_file, "digraph {\n");
  fprintf(dot_file, "  node  [ nodesep=1.5 ];\n");
  fprintf(dot_file, "  graph [ overlap=false; bgcolor=\"grey\" ];\n");
  fprintf(dot_file, "  edge  [ weight=0.5 ];\n");

  for(uint32_t i=0; i<graph.vertex_count; ++i) {
    Node* node = (Node*)(graph.root + i);
    LOG_TRACE("Printing %u : %s", i, node->name);

    if (i == 0 || i == graph.vertex_count-1)
      fprintf_node_dot_format(dot_file, node, "cyan");
    else if (is_leaf_node(node))
      fprintf_node_dot_format(dot_file, node, "gold");
    else if (is_leaf_node_ignore_back(node))
      fprintf_node_dot_format(dot_file, node, "orange");
    else
      fprintf_node_dot_format(dot_file, node, NULL);

    for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
      Node* child = follow_edge(node->slots[slot]);
      if (child)
        if (is_backwards(node->slots[slot]))
          fprintf_edge_dot_format(dot_file, node, child, "red");
        else if (get_flags_on_edge(node->slots[slot], SET_TRAVERSE_FLAG))
          fprintf_edge_dot_format(dot_file, node, child, "blue");
        else if (get_flags_on_edge(node->slots[slot], SET_VISIT_FLAG))
          fprintf_edge_dot_format(dot_file, node, child, "green4");
        else
          fprintf_edge_dot_format(dot_file, node, child, NULL);
      else if (is_backwards(node->slots[slot]))
        fprintf_edge_dot_format(dot_file, node, NULL, "red");
    }
  }

  fprintf(dot_file, "}\n");
  fclose(dot_file);
}

