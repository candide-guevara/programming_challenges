#include <special_traversals.h>

#include <stdlib.h>
#include <sys/sdt.h>

#include <logger.h>
#include <common.h>
#include <util.h>

// We expected all node in the graph have count == 0
void destructive_pointer_reversal_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor) {
  // we use poison to avoid an end of traversal condition branch
  Node poison = build_node_with_name("poison");
  poison.slots[0] = &poison;
  poison.count = 1;
  // a pointer to the start of the linked list containing the visited nodes
  Node* previous = &poison;

  // Invariants :
  // 1. node points to the current node
  // 2. node->count is the index of the first un-traversed edge of the current node
  // 3. previous points to the node visited just before
  // 4. previous>slots[previous->count - 1] points to the node visited 2 steps before
  while (node != &poison) {
    uint32_t next_slot = node->count;
    // If count > 0 we have already visited this node, we also use it to index slots[]
    if (node->count == 0)
      visitor(visit_state, node);

    for(; next_slot < SLOT_COUNT && !node->slots[next_slot]; ++next_slot);
    if (next_slot == SLOT_COUNT) {
      node->count = next_slot;
      for(next_slot=previous->count-1; next_slot >= 0 && !previous->slots[next_slot]; --next_slot);
      
      Node* tmp_node = previous;
      node = previous;
      previous = tmp_node->slots[next_slot];
      // we can only go back through this edge once
      tmp_node->slots[next_slot] = NULL;
    }
    else {  
      Node* tmp_node = node;
      node = tmp_node->slots[next_slot];
      tmp_node->slots[next_slot] = previous;
      previous = tmp_node;
      tmp_node->count = next_slot + 1;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void destructive_std_depth_first_traversal_helper(
    Node* node, VisitorState* visit_state, Visitor_t visitor, uint32_t stack_depth) {
  // We add a max recursion depth to avoid overflow even if it changes traversal order
  if (!node || node->count == 1) return;
  if (stack_depth > 8192) {
    MY_DTRACE_PROBE1(prune_branch, stack_depth);
    return; //place probe here to detect branch pruning
  }
  LOG_TRACE("Visit : %s", node->name);
  visitor(visit_state, node);
  node->count = 1;

  for(uint32_t slot=0; slot < SLOT_COUNT; ++slot) {
    Node* child = node->slots[slot];
    destructive_std_depth_first_traversal_helper(child, visit_state, visitor, stack_depth+1);
  }
}

void destructive_std_depth_first_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor) {
  destructive_std_depth_first_traversal_helper(node, visit_state, visitor, 0);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

void pointer_reversal_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor) {
  Pr_TraverseState state = pr_prepare_initial_state__mut(node, visit_state, visitor);

  // Invariant : state.current points to the next node to visit
  // Invariant : state.next_slot is the next edge to follow (or SLOT_COUNT if we have to ga back)
  while (state.current != state.poison) {
    pr_debug_pointer_traversal_state(state, visit_state);
    ASSERT(pr_get_visit_tag_value(state.current) != state.tag_token, 
           "Node %s was already visited", state.current->name);

    visitor(visit_state, state.current);
    pr_toggle_visit_tag_value(state.current);

    if(state.next_slot == SLOT_COUNT)
      // If we cannot go forward we backdash until we find an untraversed edge
      // While going back we rollback all nodes which edges are fully traversed
      pr_backward_invert_edges__mut(&state);

    if(state.next_slot < SLOT_COUNT)
      pr_advance_to_next_child__mut(&state);
  }

  pr_debug_pointer_traversal_state(state, visit_state);
  free(state.poison);
}

Pr_TraverseState pr_prepare_initial_state__mut(Node *root, VisitorState* visit_state, Visitor_t visitor) {
  Pr_TraverseState state = {0};
  state.poison = malloc(sizeof(Node));
  build_node_in_place_with_name(state.poison, "poison");

  if (root) {
    state.tag_token = pr_get_visit_tag_value(root) ^ SET_VISIT_FLAG;
    state.current = root;
    state.next_slot = pr_next_untraversed_slot(state.current, state.tag_token, 0);

    // we tag the poison node as unvisited and create a backward link to itself
    // this will be useful to detect the end of traversal condition
    state.poison->slots[0] = set_flags_on_edge(state.poison, SET_ALL_FLAG ^ state.tag_token);
  }
  else {
    state.current = state.poison;
  }
  state.previous = state.poison;
  return state;
}

void pr_advance_to_next_child__mut(Pr_TraverseState* state) {
  uint32_t next_slot = state->next_slot;
  Node** cur_slots = state->current->slots;
  ASSERT(!is_backwards(cur_slots[next_slot]), "At %s we are going backward instead of forward", 
         ((Node*)follow_edge(cur_slots[next_slot]))->name);
  Node* tmp_grand_pa = state->previous;

  state->previous = state->current;
  state->current = follow_edge(cur_slots[next_slot]);
  state->previous->slots[next_slot] = set_flags_on_edge(tmp_grand_pa, SET_TRAVERSE_FLAG|SET_BACK_FLAG|state->tag_token);
  state->next_slot = pr_next_untraversed_slot(state->current, state->tag_token, 0);
}

void pr_backward_invert_edges__mut(Pr_TraverseState *state) {
  // Invariant : state.current MUST be rolled back
  while (state->next_slot == SLOT_COUNT && state->current != state->poison) {
    uint32_t back_slot = 0;
    Node** prev_slots = state->previous->slots;
    Node* tmp_last_cur = state->current;
    pr_rollback_node_to_init_state__mut(state->current, state->tag_token);

    for(; back_slot < SLOT_COUNT 
          && !get_flags_on_edge(prev_slots[back_slot], SET_BACK_FLAG);
        ++back_slot);
    ASSERT(get_flags_on_edge(prev_slots[back_slot], SET_TRAVERSE_FLAG), 
           "All back edge should have been traversed at : %s", state->previous->name);
    ASSERT(back_slot < SLOT_COUNT, "Could not find any back edge on %s", state->previous->name);

    state->current = state->previous;
    state->previous = follow_edge(prev_slots[back_slot]);
    state->current->slots[back_slot] = set_flags_on_edge(tmp_last_cur, SET_TRAVERSE_FLAG|state->tag_token);
    state->next_slot = pr_next_untraversed_slot(state->current, state->tag_token, back_slot+1);
  }
}

void pr_rollback_node_to_init_state__mut(Node *node, size_t tag_token) {
  for(uint32_t next_slot=0; next_slot < SLOT_COUNT; ++next_slot) {
    ASSERT(!get_flags_on_edge(node->slots[next_slot], SET_BACK_FLAG),
           "There should not be any back edge when rolling back : %s", node->name);
    node->slots[next_slot] = clear_flags_on_edge(node->slots[next_slot], SET_TRAVERSE_FLAG|SET_BACK_FLAG);
  }
  ASSERT(pr_get_visit_tag_value(node) == tag_token, 
         "Rollback should keep the visited tag : %s", node->name);
}

uint32_t pr_next_untraversed_slot(Node *node, size_t tag_token, uint32_t start_slot) {
  uint32_t next_slot = start_slot;
  for(; next_slot < SLOT_COUNT; ++next_slot) {
    Node *child = follow_edge(node->slots[next_slot]);
    if (!child) continue;
    if (get_flags_on_edge(node->slots[next_slot], SET_TRAVERSE_FLAG)) continue;
    // the edge should not point to an already visited node
    if (pr_get_visit_tag_value(child) == tag_token) continue; 
    // Do not follow edges that loop back to itself
    if (child == node) continue; 
    break;
  }

  ASSERT(next_slot == SLOT_COUNT || follow_edge(node->slots[next_slot]), 
         "Untraversed null edge at %s slot %u", node->name, next_slot);
  return next_slot;
}

size_t pr_get_visit_tag_value(Node *node) {
  // Both the first AND all non null edges should have the visit tag set, we just need to check one
  return get_flags_on_edge(node->slots[0], SET_VISIT_FLAG);
}

void pr_toggle_visit_tag_value(Node *node) {
  node->slots[0] = toggle_flags_on_edge(node->slots[0], SET_VISIT_FLAG);
}

void pr_debug_pointer_traversal_state(Pr_TraverseState state, VisitorState* visit_state) {
  //#define TRAVERSE_TRACE
  #ifdef TRAVERSE_TRACE
  char filepath[128];
  static uint32_t __NAME_ID__ = 0;

  LOG_INFO("state : %p, %p, %u, %lu", state.current, state.previous, state.next_slot, state.tag_token);
  LOG_INFO("current : %s", print_node(state.current));
  LOG_INFO("previous : %s\n", print_node(state.previous));

  if(visit_state) {
    Node *root = visit_state->graph.root;
    #pragma GCC diagnostic ignored "-Wformat-truncation"
    snprintf(filepath, sizeof(filepath), "graph_%s_%u_%s_%u.dot", 
              root->name, __NAME_ID__++, state.current->name, state.next_slot);
    #pragma GCC diagnostic pop
    dump_graph_dot_format(visit_state->graph, filepath);
  }
  #endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

void destructive_pointer_back_and_forth_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor) {
  Bf_TraverseState state = bf_prepare_initial_state__mut(node, visit_state, visitor);
  if (!state.queue_len) return;

  while (1) {
    bf_debug_pointer_traversal_state(state, visit_state);
    uint32_t visit_count = bf_visit_childs_tag_generation__mut(state.tail, visit_state, visitor, state.gen);
    bf_adjust_state_based_on_nodes_visited__mut(&state, visit_count);
    if (!state.queue_len) return;

    // we go backward then we pivot and move forward looking for the new tail node
    Bf_EdgeParams params = {0};
    uint32_t tail_gen = 0;

    if (!visit_count)
      tail_gen = bf_backward_prune_exhausted_branch__mut(&state);
    if (tail_gen != state.next_tail_gen)
      params = bf_backward_invert_edges__mut(&state);

    bf_pivot_back_forward_edges__mut(&state, params);
    bf_forward_invert_edges__mut(&state);
  }
  bf_debug_pointer_traversal_state(state, visit_state);
}

uint32_t bf_backward_prune_exhausted_branch__mut(Bf_TraverseState *state) {
  Bf_EdgeParams params = {0};
  // Invariant : the tail node has no forward edges
  while (!params.min_gen) {
    ASSERT(state->parent, "We should not go past the root while pruning backwards");
    params = bf_get_node_edge_parameters(state->parent);
    ASSERT(is_backwards(state->parent->slots[params.inv_idx]), "There should always be a backward edge");

    state->tail = state->parent;
    state->parent = follow_edge(state->parent->slots[params.inv_idx]);
  }
  // we reached a node with ancestors in the visit queue, recalculate generation after prune
  state->tail->count = params.min_gen;
  // we keep the invariant that between parent and tail there is no edge
  state->tail->slots[params.inv_idx] = NULL;
  ASSERT(state->tail->count >= state->next_tail_gen, "When we cannot prune further we should be at a higher generation");
  return state->tail->count;
}

void bf_adjust_state_based_on_nodes_visited__mut(Bf_TraverseState *state, uint32_t visit_count) {
  state->gen += visit_count;
  state->queue_len += visit_count - 1;
  state->next_tail_gen += 1;
  ASSERT(state->queue_len < state->gen, "generation > queue_len");
  ASSERT(state->queue_len >= 0 , "We should have stopped before queue_len < 0");
}

Bf_TraverseState bf_prepare_initial_state__mut(Node *root, VisitorState* visit_state, Visitor_t visitor) {
  Bf_TraverseState state = {0};
  if (!root) return state;
  bf_debug_pointer_traversal_state(state, visit_state);

  visitor(visit_state, root);
  state.parent = root;
  // we set the root generation now in case it has edges pointing to itself
  state.parent->count = 1;
  uint32_t visit_count = bf_visit_childs_tag_generation__mut(root, visit_state, visitor, 1);

  if (visit_count) {
    uint32_t tail_idx=0;
    for(; tail_idx<SLOT_COUNT && root->slots[tail_idx] == NULL; ++tail_idx);

    state.tail = root->slots[tail_idx];
    state.queue_len = visit_count;
    state.gen = visit_count + 1;
    state.next_tail_gen = 2;

    root->slots[tail_idx] = to_backwards_edge(NULL);
  }
  return state;
}

uint32_t bf_visit_childs_tag_generation__mut(Node* node, VisitorState* visit_state, Visitor_t visitor, uint32_t start_gen) {
  uint32_t visit_count = 0;

  for(uint32_t i=0; i<SLOT_COUNT; ++i) {
    ASSERT(!is_backwards(node->slots[i]), "The tail node can only have edges going forward");
    if (node->slots[i]) {
      Node *child = node->slots[i];
      // cycle detected, remove edge to avoid recursion
      if (child->count) {
        ASSERT(child->count <= start_gen, "Weird cycle to the future");
        node->slots[i] = NULL;
      }
      else {
        visit_count += 1;
        child->count = start_gen + visit_count;
        visitor(visit_state, child);
      }
    }
  }
  // A node has the same generation as the smallest of its visited ancestors
  if(visit_count)
    node->count = start_gen + 1;
  return visit_count;
}

Bf_EdgeParams bf_backward_invert_edges__mut(Bf_TraverseState *state) {
  Bf_EdgeParams params = {0};
  Node *grand_pa = NULL;

  // Invariant : (grand_pa / NULL) <-- (parent)   (tail)
  while(1) {
    params = bf_get_node_edge_parameters(state->parent);
    ASSERT(state->parent->count <= state->tail->count, "While going backwards children always hold a higher gen than parents");
    ASSERT(is_backwards(state->parent->slots[params.inv_idx]), "There should always be a backward edge");
    grand_pa = follow_edge(state->parent->slots[params.inv_idx]);

    state->parent->count = params.min_gen ? params.min_gen : state->tail->count;
    ASSERT(state->parent->count >= state->next_tail_gen, "While going backwards we never go under the target generation");

    // we have found the closest ancestor of the node we are looking for
    if (state->parent->count == state->next_tail_gen) break;

    state->parent->slots[params.inv_idx] = state->tail;
    state->tail = state->parent;
    state->parent = grand_pa;
    ASSERT(state->parent, "We should not go past the root while going backwards");
  }
  // The edge params of the node where the traversal must change direction
  // Or the default value on a degenerate case : no need to backward to reach the next tail node
  return params;
}

void bf_pivot_back_forward_edges__mut(Bf_TraverseState *state, Bf_EdgeParams params) {
  // degenerate case : no need to backward to reach the next tail node
  if (!params.min_gen) return;

  Node *grand_pa = follow_edge(state->parent->slots[params.inv_idx]);
  state->parent->slots[params.inv_idx] = state->tail;
  state->tail = state->parent->slots[params.min_idx];
  state->parent->slots[params.min_idx] = to_backwards_edge(grand_pa);
  ASSERT(state->tail->count == state->next_tail_gen, "The pivoted edge should have pointed to the next tail node");
}

void bf_forward_invert_edges__mut(Bf_TraverseState *state) {
  while(1) {
    ASSERT(state->tail->count == state->next_tail_gen, "Going forward we only traverse ancestors of the target tail");
    Bf_EdgeParams params = bf_get_node_edge_parameters(state->tail);

    // we have found a node whose children have not been visited that has the generation we want 
    if (!params.min_gen 
        // or whose children lowest gen does not match (this indicates a cycle to an already visited node)
        || params.min_gen != state->next_tail_gen
        || bf_pathological_branch_loop_back(state->tail, params.min_idx) ) 
      // => this is the tail
      break;

    Node *grand_pa = state->parent;
    state->parent = state->tail;
    state->tail = state->tail->slots[params.min_idx];
    state->parent->slots[params.min_idx] = to_backwards_edge(grand_pa);
  }
}

Bf_EdgeParams bf_get_node_edge_parameters(Node *node) {
  Bf_EdgeParams params = {0};

  for(uint32_t i=0; i<SLOT_COUNT; ++i) {
    if (is_backwards(node->slots[i])) {
      ASSERT(params.inv_idx == i || !is_backwards(node->slots[params.inv_idx]), "There can only be one backward edge");
      params.inv_idx = i;
    }
    else if (node->slots[i] && (!params.min_gen || params.min_gen > node->slots[i]->count)) {
      params.min_idx = i;
      params.min_gen = node->slots[i]->count;
    }
  }
  return params;
}

uint32_t bf_pathological_branch_loop_back(Node *node, uint32_t child_edge) {
  Node *child = node->slots[child_edge];
  uint32_t is_a_loop = child == node;
  // The inverted edge we write while traversing forward serves as a "breadcrumb" 
  // to identify the previous nodes of this branch
  for(uint32_t i=0; i<SLOT_COUNT && !is_a_loop; ++i) 
    if (is_backwards(child->slots[i])) {
      MY_DTRACE_PROBE2(patho_branch_loop, node, child_edge);
      is_a_loop = 1;
    }
  return is_a_loop;
}

void bf_debug_pointer_traversal_state(Bf_TraverseState state, VisitorState* visit_state) {
  #ifdef TRAVERSE_TRACE
  char filepath[128];

  LOG_INFO("state : %p, %p, %u, %u, %u", state.tail, state.parent, state.gen, state.next_tail_gen, state.queue_len);
  LOG_INFO("tail : %s", print_node(state.tail));
  LOG_INFO("parent : %s\n", print_node(state.parent));

  if (visit_state) {
    Node *root = visit_state->graph.root;
    snprintf(filepath, sizeof(filepath), "graph_%s_%u_%u_%u.dot", 
              root->name, state.gen, state.next_tail_gen, state.queue_len);
    dump_graph_dot_format(visit_state->graph, filepath);
  }
  #endif
}

