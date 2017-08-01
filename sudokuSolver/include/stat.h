#ifndef _STAT_H_
#define _STAT_H_
  #include <types.h>

  Stat* getStat();
  Stat *calculateGlobalStat();
  void allocateStat(MY_TYPE number);
  void nextStat();

#endif
