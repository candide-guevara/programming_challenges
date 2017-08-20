#include <node.h>

#include <stdlib.h>
#include <string.h>

#include <logger.h>
#include <common.h>

static const char* DICTIONARY[] = {
  "monkey", "chimp", "gorilla", "orangutan", "bonobo", "gibbon", "macaque", 
  "baboon", "mandrill", "tamarin", "saimiri", "capuchin", "nim_chimpsky",
  "marmoset", "alouatta", "uakari"
};
static const uint32_t DICT_LEN = sizeof(DICTIONARY) / sizeof(const char*);

void build_node_in_place_with_name(Node* node, const char* name) {
  memset(node, 0, sizeof(Node));
  strncpy(node->name, name, (size_t)NAME_LEN);
  LOG_TRACE("Built node %s", node->name);
}

void build_node_in_place(Node* node, uint32_t id) {
  memset(node, 0, sizeof(Node));
  strncpy(node->name, give_me_random_name(id), (size_t)NAME_LEN);
  LOG_TRACE("Built node %s", node->name);
}

Node build_node (uint32_t id) {
  Node n;
  build_node_in_place(&n, id);
  return n;
}

Node build_node_with_name (const char *name) {
  Node n;
  build_node_in_place_with_name(&n, name);
  return n;
}

///////////////////////////////////////////////////////////////////////////////////////////////

const char* print_node (Node* node) {
  static char __BUFFER__[256];
  if (!node) return "NULL";
  size_t written_count = snprintf(__BUFFER__, sizeof(__BUFFER__), "Node::%s[%d] ", 
                                  node->name, node->count);
  print_slots((Node*)node, __BUFFER__ + written_count, sizeof(__BUFFER__) - written_count);
  return __BUFFER__;
}

// we expect that buffer[0] == '\0'
void print_slots(Node* node, char* buffer, int32_t buffer_len) {
  if (buffer_len < 2) return;
  buffer[0] = '(';
  buffer_len -= 1;
  buffer += 1;

  // Invariant : buffer[0] == '\0'
  buffer[0] = '\0';
  for(uint32_t slot=0; slot < SLOT_COUNT && buffer_len > 1; ++slot) {
    size_t written_count = snprintf(buffer, buffer_len, "%p, ", node->slots[slot]);
    buffer_len -= written_count;
    buffer += written_count;
  }

  if (buffer_len > 1) {
    buffer[0] = ')';
    buffer[1] = '\0';
  }
}

const char* give_me_random_name (uint32_t id) {
  static char __BUFFER__[NAME_LEN] = { '\0' };

  const char* name1 = DICTIONARY[ rand() % DICT_LEN ];
  const char* name2 = DICTIONARY[ rand() % DICT_LEN ];

  snprintf(__BUFFER__, sizeof(__BUFFER__), "%s.%s_%d", 
           name1, name2, id);
  return __BUFFER__;
}

