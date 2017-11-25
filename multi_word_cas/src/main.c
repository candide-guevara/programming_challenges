#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <benchmark.h>
#include <common.h>
#include <logger.h>
#include <test_read.h>
#include <test_write.h>

int main(void) {
  LOG_WARN("Starting ...");
  main_common_unit_test();
  main_read_unit_test();
  main_write_unit_test();
  //main_run_benchmark();
  LOG_WARN("ALL DONE");
  return 0;
}
