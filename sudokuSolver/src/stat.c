#include <stdlib.h>

#include <types.h>

Stat *stats = NULL;
Stat globalStat = { 0, 0 };
int current = -1;

Stat* getStat() {
  if(!stats) return NULL;
  return stats + current;
}

Stat *calculateGlobalStat() {
  MY_TYPE i;
  globalStat.combinations = 0;
  globalStat.recursions = 0;
  if (stats) {
    for(i=0; i<=current; ++i) {
      globalStat.combinations += stats[i].combinations;
      globalStat.recursions += stats[i].recursions;
    }
  }
  return &globalStat;
}

void allocateStat(MY_TYPE number) {
  if (stats) {
    free(stats);
  }
  stats = calloc(number, sizeof(Stat));
  current = -1;
}

void nextStat() {
  if (!stats) return;
  ++current;
  stats[current].combinations = 0;
  stats[current].recursions = 0;
}

