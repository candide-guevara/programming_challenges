#include <stdlib.h>
#include <stdio.h>

#include <common.h>
#include <logger.h>

typedef struct Control Control;
typedef union CtrlView CtrlView;

struct Control {
  unsigned int latest_slot:4;
  unsigned int contending_slot:4;
  unsigned int reader_1_lock:4;
  unsigned int reader_2_lock:4;
  unsigned int reader_3_lock:4;
  unsigned int reader_4_lock:4;
};

union CtrlView {
  unsigned long long raw;
  Control fields;
};
STATIC_ASSERT(sizeof(CtrlView) == sizeof(unsigned long long), control_too_big);

int main (void) {
  LOG_WARN("Starting ...");
  LOG_INFO("All done !!");
  return 0;
}

