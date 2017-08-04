#pragma once

#include <stdint.h>
#include <stdio.h>

enum __node_h__ { 
  NAME_LEN = 64,
  SLOT_COUNT = 4, // the number of pointers per node
  POINTER_DENSITY_PERC = 25, // the probability that a node pointer is NOT null
  CYCLE_DENSITY_PERC = 30,   // the probability of replacing a null node pointer in a DAG vertex for a cycle
};

typedef struct Node Node;
typedef struct CountedNode CountedNode;

struct Node {
  char name[NAME_LEN];
  Node* slots[SLOT_COUNT];
};

struct CountedNode {
  Node data;
  uint32_t count;
};

Node build_node(const char* name);
CountedNode build_counted_node(const char* name);
void build_node_in_place(Node* node, const char* name);
void build_counted_node_in_place(CountedNode* node, const char* name);

const char* print_node(Node* node);
const char* print_counted_node(CountedNode* node);
void print_slots(Node* node, char* buffer, int32_t buffer_len);

const char* give_me_random_name();

