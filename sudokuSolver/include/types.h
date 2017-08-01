#ifndef _TYPES_H_
#define _TYPES_H_

  #define _GOOD_OPT_
  //#define _NO_OPT_

  #define NUMBER_OF_GRIDS 10
  #define GRIDS_DIR "resource/grids"

  #define NO_NUMBER 10
  #define END_OF_STR '\0'
  #define INT_FORMAT "u"
  #define ASCII_ZERO 48

  #define DO_PRINT 3
  #define LOG_TRACE if(1 >= DO_PRINT)
  #define LOG_DEBUG if(2 >= DO_PRINT)
  #define LOG_INFO  if(3 >= DO_PRINT)
  #define LOG_WARN  if(4 >= DO_PRINT)
  #define LOG_ERROR if(5 >= DO_PRINT)

  typedef int MY_TYPE;

  struct Stat_Struct {
    int recursions;
    int combinations;
  };
  typedef struct Stat_Struct Stat;

  struct Buffer_Struct {
    char buf[1024*128];
    char* current;
  };
  typedef struct Buffer_Struct Buffer;

  static const char _FILES_[NUMBER_OF_GRIDS][64] = {
    GRIDS_DIR "/regularGrid", 
    GRIDS_DIR "/easy1", 
    GRIDS_DIR "/easy2", 
    GRIDS_DIR "/easy3", 
    GRIDS_DIR "/medium1", 
    GRIDS_DIR "/medium2", 
    GRIDS_DIR "/medium3", 
    GRIDS_DIR "/hard1", 
    GRIDS_DIR "/hard2", 
    GRIDS_DIR "/hard3", 
  };

  static const MY_TYPE COMBINATION[] = {1,2,3,4,5,6,7,8,9};
#endif
