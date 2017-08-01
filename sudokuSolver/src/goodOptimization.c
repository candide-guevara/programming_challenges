#include <types.h>
#ifdef _GOOD_OPT_

#include <stdio.h>
#include <string.h>

const MY_TYPE FREQUENCY[] = {1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,0};

MY_TYPE *getCandidate(MY_TYPE *grid, MY_TYPE *move) {
  MY_TYPE maxRow= 0, maxCol = 0;
  MY_TYPE tCol, tRow, i, j, k, l;
  MY_TYPE minIndex = 1;
  move[0] = NO_NUMBER;
  
  for(i=0, j=0; i<73; i+=9, ++j) {
    tCol = tRow = 0;
    for(k=0, l=0; k<9; ++k, l+=9) {
      if (grid[i+k]) ++tRow;
      if (grid[j+l]) ++tCol;
    }
    if (tRow < 9 && maxRow <= tRow) {
      move[0] = j;
      maxRow = tRow;
    }
    if (tCol < 9 && maxCol <= tCol) {
      move[1] = j;
      maxCol = tCol;
    }
  }

  if (move[0] == NO_NUMBER) return NULL;
  if (grid[move[0]*9+move[1]] == 0) return move;
  if (maxCol > maxRow) {
    minIndex = 0;
  }
  for(move[minIndex]=0; grid[move[0]*9+move[1]]; ++move[minIndex]);
  return move;
}

MY_TYPE *getProposals(MY_TYPE *grid, MY_TYPE *move, MY_TYPE *comb) {
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

void getCornerFrequency(MY_TYPE *grid, MY_TYPE *move, MY_TYPE *freq) {
  MY_TYPE xr = 0, xl = 8, yu = 8, yd = 0;
  memcpy(freq, FREQUENCY, sizeof(MY_TYPE)*19);

  if (move[0] < 8) xr = move[0]+1;
  if (move[0]) xl = move[0]-1;
  if (move[1] < 8) yd = move[1]+1;
  if (move[1]) yu = move[0]-1;

  MY_TYPE ul = grid[xl*9+yu];
  MY_TYPE dl = grid[xl*9+yd];
  MY_TYPE ur = grid[xr*9+yu];
  MY_TYPE dr = grid[xr*9+yd];

  if(ul) { ++freq[ul*2-1]; ++freq[18]; }
  if(ur) { ++freq[ur*2-1]; ++freq[18]; }
  if(dl) { ++freq[dl*2-1]; ++freq[18]; }
  if(dr) { ++freq[dr*2-1]; ++freq[18]; }
}

void sortProposals(MY_TYPE *comb, MY_TYPE *freq) {
  if(!freq[18]) return;
  MY_TYPE i, f, tmp;

  for(i=0, f=0; i<18; i+=2) {
    f = freq[i+1]; 
    if (!f || !comb[freq[i]-1]) continue;
    if (f <= freq[18]-1) {
      tmp = comb[freq[18]-1-f];
      comb[freq[18]-1-f] = freq[i];
      comb[freq[i]-1] = tmp;
    }
    else {
      tmp = comb[0];
      comb[0] = freq[i];
      comb[freq[i]-1] = tmp;
    }
  }
}

/*void sortProposals(MY_TYPE *comb, MY_TYPE *freq) {
  if(!freq[18]) return;
  MY_TYPE i, f, tmp;

  for(i=0, f=0; i<18; i+=2) {
    f = freq[i+1]; 
    if (!f || !comb[freq[i]-1]) continue;
    tmp = comb[8-freq[18]+f];
    comb[8-freq[18]+f] = freq[i];
    comb[freq[i]-1] = tmp;
  }
}*/

MY_TYPE *possibleCombinations(MY_TYPE *grid, MY_TYPE *move, MY_TYPE *comb) {
  MY_TYPE freq[19];
  if (getProposals(grid, move, comb)) {
    getCornerFrequency(grid, move, freq);
    sortProposals(comb, freq);
    return comb;
  }
  else return NULL;
}

#endif
