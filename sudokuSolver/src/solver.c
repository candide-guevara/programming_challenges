#include <string.h>

#include <types.h>
#include <optimization.h>

static MY_TYPE validGrid(MY_TYPE *grid) {
  MY_TYPE i, j, k, l;
  MY_TYPE comb[9], comb2[9];
  
  for(i=0, j=0; i<73; i+=9, ++j) {
    memcpy(comb, COMBINATION, 9*sizeof(MY_TYPE));
    memcpy(comb2, COMBINATION, 9*sizeof(MY_TYPE));
    for(k=0, l=0; k<9; ++k, l+=9) {
      if (grid[i+k]) {
        if (!comb[grid[i+k]-1]) return 0;
        comb[grid[i+k]-1] = 0;
      }
      if (grid[j+l]) {
        if (!comb2[grid[j+l]-1]) return 0;
        comb2[grid[j+l]-1] = 0;
      }
    }
  }
  return 1;
}

MY_TYPE performMove(MY_TYPE *grid, Stat *stats) {
  if (!validGrid(grid)) return 0;

  MY_TYPE move[2];
  MY_TYPE combinations[9];
  MY_TYPE initGrid[81];
  MY_TYPE i;
  memcpy(initGrid, grid, 81*sizeof(MY_TYPE));
  ++(*stats).recursions;

  if (!getCandidate(grid, move)) return 1;
  if (!possibleCombinations(grid, move, combinations)) return 0;

  for(i=0; i<9; i++) {
    if (!combinations[i]) continue;
    ++(*stats).combinations;
    grid[move[0]*9+move[1]] = combinations[i];
    if (performMove(grid, stats)) return 1;
    --(*stats).recursions;
    memcpy(grid, initGrid, 81*sizeof(MY_TYPE));
  }
  return 0;
}

