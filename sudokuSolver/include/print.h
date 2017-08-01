#ifndef _PRINT_H_
#define _PRINT_H_

  #include <types.h>

  void initBuffer();
  void myprint(const char *s);
  void myprint2(const char *s1, const char *s2);
  void myprint3(const char *s1, const char *s2, const char *s3);
  void printGrid(MY_TYPE *grid);
  void printStat(Stat *s);
  void flush();

#endif

