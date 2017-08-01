#include <types.h>
#include <stdio.h>
#include <string.h>

Buffer buffer;

void initBuffer() {
  buffer.current = buffer.buf;
  buffer.buf[0] = END_OF_STR;
}

void flush() {
  LOG_ERROR printf("%s", buffer.buf);
  initBuffer();
}

void myprint(const char *s) {
  strcpy(buffer.current, s);
  buffer.current += strlen(s);
}

void myprint2(const char *s1, const char *s2) {
  myprint(s1);
  myprint(s2);
}

void myprint3(const char *s1, const char *s2, const char *s3) {
  myprint(s1);
  myprint(s2);
  myprint(s3);
}

void printGrid(MY_TYPE *grid) {
  char s[19*9+2];
  int i, j, k, l;

  for (i=0, j=0; i<19*9; i+=19, j+=9) {
    for (k=0, l=0; k<18; k+=2, ++l) {
      s[i+k] = ASCII_ZERO + grid[j+l];
      s[i+k+1] = ' ';
    }
    s[i+18] = '\n';
  }
  s[19*9] = '\n';
  s[19*9*+1] = END_OF_STR;
  myprint(s);
}

void printStat(Stat *s) {
  Stat stat = *s;
  float average = stat.recursions ? (float)stat.combinations / stat.recursions : 0.0;
  char tmp[256];
  sprintf(tmp, "Statistics :\n - Combinations tested : %d\n - PerformMove calls : %d\n - Average combination per call : %f\n",
    stat.combinations, stat.recursions, average);
  myprint(tmp);
}


