#ifndef LOGGING_H_
#define LOGGING_H_

#include <iostream>

#ifndef __LEVEL_LOG__
  #define __LEVEL_LOG__ 4
#endif

#ifndef __COLOR_OUTPUT__
  #define __COLOR_OUTPUT__ 1
#endif

#define __LOG__(ostream, level, color, msg) \
  (ostream << __LOG_HEADER__(level,color) << " " << msg << std::endl)
#define __LOG_HEADER__(level, color) \
  color << "[" << level << "] " << EGM::COL_BOLDWHITE << __FILE__  << ":" << __LINE__ << EGM::COL_RESET

#if __LEVEL_LOG__ > 3
  #define LOG_DEBUG(msg) __LOG__(std::cout,"DEBG",EGM::COL_GREEN,msg)
  #define POPS __LOG__(std::cout,"DEBG",EGM::COL_GREEN,"<-- POPS -->")
  #define STL_VERBOSE 1 
#else
  #define LOG_DEBUG(msg)
  #define POPS
  #define STL_VERBOSE 0 
#endif

#if __LEVEL_LOG__ > 2
  #define LOG_INFO(msg) __LOG__(std::cout,"INFO",EGM::COL_CYAN,msg)
#else
  #define LOG_INFO(msg)
#endif

#if __LEVEL_LOG__ > 1
  #define LOG_WARN(msg) __LOG__(std::cerr,"WARN",EGM::COL_YELLOW,msg)
  #define LOG_MONIT(msg) __LOG__(std::cerr,"MONIT",EGM::COL_CYAN,msg)
#else
  #define LOG_WARN(msg)
  #define LOG_MONIT(msg)
#endif

#if __LEVEL_LOG__ > 0
  #define LOG_ERROR(msg) __LOG__(std::cerr,"FATL",EGM::COL_BOLDRED,msg)
  #define LOG_ASSERT(exp,msg) __LOG__(std::cerr,"ASSERT",EGM::COL_RED,#exp << " : " << msg)
#else
  #define LOG_ERROR(msg)
  #define LOG_ASSERT(msg)
#endif

namespace EGM {
  const static std::ios RESET_STREAM(NULL);

  extern const char* COL_RESET         ;
  extern const char* COL_BLACK         ;
  extern const char* COL_RED           ;
  extern const char* COL_GREEN         ;
  extern const char* COL_YELLOW        ;
  extern const char* COL_BLUE          ;
  extern const char* COL_MAGENTA       ;
  extern const char* COL_CYAN          ;
  extern const char* COL_WHITE         ;
  extern const char* COL_BOLDBLACK     ;
  extern const char* COL_BOLDRED       ;
  extern const char* COL_BOLDGREEN     ;
  extern const char* COL_BOLDYELLOW    ;
  extern const char* COL_BOLDBLUE      ;
  extern const char* COL_BOLDMAGENTA   ;
  extern const char* COL_BOLDCYAN      ;
  extern const char* COL_BOLDWHITE     ;
} // namespace EGM

#endif /* LOGGING_H_ */
