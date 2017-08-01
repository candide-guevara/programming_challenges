#include <types.h>
#ifdef _NO_OPT_

#include <stdio.h>
#include <string.h>

MY_TYPE *getCandidate(MY_TYPE *grid, MY_TYPE *move) {
  MY_TYPE i, j, k;
  for (i=0, k=0; i<73; i+=9, ++k) {
    for (j=0; j<9; ++j) if (!grid[i+j]) {
      move[0] = k;
      move[1] = j;
      return move;
    }
  }
  return NULL;
}

MY_TYPE *possibleCombinations(MY_TYPE *grid, MY_TYPE *move, MY_TYPE *comb) {
  MY_TYPE temp = 0, i;

  memcpy(comb, COMBINATION, 9*sizeof(MY_TYPE));
  for (i=0; i<9; ++i) {
    temp = grid[move[0]*9+i];
    if(temp) comb[temp-1] = 0;
    temp = grid[i*9+move[1]];
    if(temp) comb[temp-1] = 0;
  }
  for(temp=0, i=0; i<9; temp+=comb[i++]);
  if (!temp) return NULL;
  return comb;
}

#endif
