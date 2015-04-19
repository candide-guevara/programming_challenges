#include <logger.hpp>

namespace EGM {
#if __COLOR_OUTPUT__ > 0
  const char* COL_RESET         =  "\033[0m";
  const char* COL_BLACK         =  "\033[30m";
  const char* COL_RED           =  "\033[31m";
  const char* COL_GREEN         =  "\033[32m";
  const char* COL_YELLOW        =  "\033[33m";
  const char* COL_BLUE          =  "\033[34m";
  const char* COL_MAGENTA       =  "\033[35m";
  const char* COL_CYAN          =  "\033[36m";
  const char* COL_WHITE         =  "\033[37m";
  const char* COL_BOLDBLACK     =  "\033[1m\033[30m";
  const char* COL_BOLDRED       =  "\033[1m\033[31m";
  const char* COL_BOLDGREEN     =  "\033[1m\033[32m";
  const char* COL_BOLDYELLOW    =  "\033[1m\033[33m";
  const char* COL_BOLDBLUE      =  "\033[1m\033[34m";
  const char* COL_BOLDMAGENTA   =  "\033[1m\033[35m";
  const char* COL_BOLDCYAN      =  "\033[1m\033[36m";
  const char* COL_BOLDWHITE     =  "\033[1m\033[37m";
#else
  const char* COL_RESET         =  "";
  const char* COL_BLACK         =  "";
  const char* COL_RED           =  "";
  const char* COL_GREEN         =  "";
  const char* COL_YELLOW        =  "";
  const char* COL_BLUE          =  "";
  const char* COL_MAGENTA       =  "";
  const char* COL_CYAN          =  "";
  const char* COL_WHITE         =  "";
  const char* COL_BOLDBLACK     =  "";
  const char* COL_BOLDRED       =  "";
  const char* COL_BOLDGREEN     =  "";
  const char* COL_BOLDYELLOW    =  "";
  const char* COL_BOLDBLUE      =  "";
  const char* COL_BOLDMAGENTA   =  "";
  const char* COL_BOLDCYAN      =  "";
  const char* COL_BOLDWHITE     =  "";
#endif

} // namespace EGM

