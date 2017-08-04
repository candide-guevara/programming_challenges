#include <node.h>

#include <common.h>

static const char* DICTIONARY[] = {
  "monkey", "chimp", "gorilla", "orangutan", "bonobo", "gibbon", "baboon", "mandrill", "tamarin", "saimiri", "capuchin", "titi", "nim_chimpsky"
};
static const uint32_t DICT_LEN = sizeof(DICTIONARY) / sizeof(const char*);

void build_node_in_place(Node* node, const char* name) {
  bzero(node, sizeof(Node));
  if (name)
    strncpy(node->name, name, (size_t)NAME_LEN);
  else
    strncpy(node->name, give_me_random_name(), (size_t)NAME_LEN);
  LOG_TRACE("Built node %s", node->name);
}

void build_counted_node_in_place(CountedNode* node, const char* name) {
  build_node_in_place((Node*)node, name);
  node->count = 0;
}

Node build_node (const char* name) {
  Node n;
  build_node_in_place(&n, name);
  return n;
}

CountedNode build_counted_node (const char* name) {
  CountedNode n;
  build_counted_node_in_place(&n, name);
  return n;
}

const char* print_node (Node* node) {
  static char __BUFFER__[256];
  size_t written_count = snprintf(__BUFFER__, sizeof(__BUFFER__), "Node::%s ", node->name);
  print_slots(node, __BUFFER__ + written_count, sizeof(__BUFFER__) - written_count);
  return __BUFFER__;
}

const char* print_counted_node (CountedNode* node) {
  static char __BUFFER__[256];
  size_t written_count = snprintf(__BUFFER__, sizeof(__BUFFER__), "CountedNode::%s[%d] ", 
                                  node->data.name, node->count);
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

const char* give_me_random_name () {
  static char __BUFFER__[NAME_LEN] = { '\0' };
  static uint32_t __ID__ = 0;

  const char* name1 = DICTIONARY[ rand() % DICT_LEN ];
  const char* name2 = DICTIONARY[ rand() % DICT_LEN ];

  snprintf(__BUFFER__, sizeof(__BUFFER__), "%s.%s_%d", 
           name1, name2, __ID__);
  __ID__++;
  return __BUFFER__;
}

