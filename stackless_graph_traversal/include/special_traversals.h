#pragma once

#include "node.h"
#include "graph.h"

void pointer_reversal_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor);
// Depth first, after traversal the graph becomes unusable
void destructive_pointer_reversal_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor);
// Inefficient breadth first
void destructive_pointer_back_and_forth_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor);
// Tweaked to handle graph cycles and avoid stack overflow
void destructive_std_depth_first_traversal(Node* node, VisitorState* visit_state, Visitor_t visitor);

///////////////////////////////////////////////////////////////////////////////////////////

struct Pr_TraverseState {
  Node *current, *previous, *poison;
  // the edge of the current node which should be traversed next
  // or SLOT_COUNT in case the current node has no more edges to follow
  uint32_t next_slot;
  size_t tag_token;
};
typedef struct Pr_TraverseState Pr_TraverseState;

Pr_TraverseState pr_prepare_initial_state__mut(Node *root, VisitorState* visit_state, Visitor_t visitor);
void pr_advance_to_next_child__mut(Pr_TraverseState* state);
void pr_backward_invert_edges__mut(Pr_TraverseState *state);
void pr_rollback_node_to_init_state__mut(Node *node, size_t tag_token);
uint32_t pr_next_untraversed_slot(Node *node, size_t tag_token, uint32_t start_slot);
size_t pr_get_visit_tag_value(Node *node);
void pr_toggle_visit_tag_value(Node *node);
void pr_debug_pointer_traversal_state(Pr_TraverseState state, VisitorState* visit_state);

///////////////////////////////////////////////////////////////////////////////////////////

struct Bf_TraverseState {
  Node *tail, *parent;
  uint32_t gen, queue_len, next_tail_gen;
};
typedef struct Bf_TraverseState Bf_TraverseState;

struct Bf_EdgeParams {
  // If min_gen == 0, then the node contains no forward edges
  uint32_t inv_idx, min_idx, min_gen;
};
typedef struct Bf_EdgeParams Bf_EdgeParams;

// if we do not prune the branch that bears no more children, we will not be able to navigate using the generation
// At the end of the pruning we need to go forwards
uint32_t bf_backward_prune_exhausted_branch__mut(Bf_TraverseState *state);

void bf_adjust_state_based_on_nodes_visited__mut(Bf_TraverseState *state, uint32_t visit_count);

Bf_TraverseState bf_prepare_initial_state__mut(Node *root, VisitorState* visit_state, Visitor_t visitor);

uint32_t bf_visit_childs_tag_generation__mut(Node* node, VisitorState* visit_state, Visitor_t visitor, uint32_t start_gen);

// While going backwards we also adjust the generation of the nodes we traverse to preserve the invariant :
// a node has the same generation as the smallest of its visited ancestors
Bf_EdgeParams bf_backward_invert_edges__mut(Bf_TraverseState *state);

// The edge pointing to the lowest generation is swapped with the edge pointing to the highest
void bf_pivot_back_forward_edges__mut(Bf_TraverseState *state, Bf_EdgeParams params);

void bf_forward_invert_edges__mut(Bf_TraverseState *state);

Bf_EdgeParams bf_get_node_edge_parameters(Node *node);

// Returns true if the node has a loop to itself
// Or we have something like : -- (gen:7) <-- (gen:7) <-- (gen:7)
//                                   ^______________________/
uint32_t bf_pathological_branch_loop_back(Node *node, uint32_t child_edge);

void bf_debug_pointer_traversal_state(Bf_TraverseState state, VisitorState* visit_state);

