#include <stdio.h>
#include <stdlib.h>

#include <types.h>
#include <print.h>
#include <solver.h>
#include <stat.h>

static void loadGrid(const char *file, MY_TYPE *grid) {
  LOG_TRACE myprint3("Loading : ", file, "\n");
  FILE *f = fopen(file, "r");
  while(fscanf(f, "%" INT_FORMAT " %" INT_FORMAT " %" INT_FORMAT " %" INT_FORMAT " %" INT_FORMAT " %" INT_FORMAT " %" INT_FORMAT " %" INT_FORMAT " %" INT_FORMAT "\n",
    grid, grid+1, grid+2, grid+3, grid+4, grid+5, grid+6, grid+7, grid+8) == 9)
    grid += 9;
  fclose(f);
}

MY_TYPE loadAllGrids(MY_TYPE ***grids) {
  (*grids) = calloc(NUMBER_OF_GRIDS, sizeof(MY_TYPE *));

  for(int i=0; i<NUMBER_OF_GRIDS; ++i) {
    (*grids)[i] = calloc(81, sizeof(MY_TYPE));
    loadGrid(_FILES_[i], (*grids)[i]);
  }
  return NUMBER_OF_GRIDS;
}

void clearAllGrids(MY_TYPE ***grids) {
  for(int i=0; i<NUMBER_OF_GRIDS; ++i)
    if( (*grids)[i] )
      free( (*grids)[i] );
  free( *grids );
}

MY_TYPE solveGrid(MY_TYPE *grid) {
  LOG_DEBUG myprint("Initial grid :\n");
  LOG_DEBUG printGrid(grid);
  nextStat();

  MY_TYPE validSolution = performMove(grid, getStat());

  if(validSolution) {
    LOG_INFO myprint("\nProblem completed \n");
    LOG_INFO printGrid(grid);
  }
  else LOG_WARN myprint("NO solution found !\n");

  LOG_WARN printStat(getStat());
  return !validSolution;
}


