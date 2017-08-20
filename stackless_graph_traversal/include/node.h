#pragma once

#include <stdint.h>
#include <stdio.h>

enum __node_h__ { 
  NAME_LEN = 64,
  SLOT_COUNT = 8, // the number of pointers per node
  POINTER_DENSITY_PERC = 40, // the probability that a node pointer is NOT null
  CYCLE_DENSITY_PERC = 25,   // the probability of replacing a null node pointer in a DAG vertex for a cycle
};

typedef struct Node Node;

struct Node {
  char name[NAME_LEN];
  Node* slots[SLOT_COUNT];
  uint32_t count;
};

Node build_node(uint32_t id);
Node build_node_with_name (const char *name);
void build_node_in_place(Node* node, uint32_t id);
void build_node_in_place_with_name(Node* node, const char* name);

const char* print_node(Node* node);
void print_slots(Node* node, char* buffer, int32_t buffer_len);

const char* give_me_random_name(uint32_t id);

