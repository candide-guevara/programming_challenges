#include <stdio.h>

#include <types.h>
#include <print.h>
#include <batchSolve.h>
#include <stat.h>

int main() {
  initBuffer();
  MY_TYPE **grids = NULL;
  MY_TYPE number = loadAllGrids(&grids);
  allocateStat(number);
  int i;

  for (i=0; i<number; ++i) {
    solveGrid(grids[i]);
  }

  LOG_WARN myprint("Global stats :\n");
  LOG_ERROR printStat(calculateGlobalStat());
  LOG_INFO myprint("Done !! \n");

  clearAllGrids(&grids);
  flush();
  return 0;
}

