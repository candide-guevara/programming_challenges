/*
 * Created on: Nov 23, 2013
 *      Author: eguevara
 */

#ifndef LOGGING_H_
#define LOGGING_H_

#include <iostream>
#include <sstream>
#include <string>
#include <cassert>

#ifndef __LEVEL_LOG__
  #define __LEVEL_LOG__ 2
#endif  

#define __LOG__(level, msg) (std::cout << "[" << level << "]" << __FILE__  << ":" << __LINE__ << " - " << msg << std::endl)

#if __LEVEL_LOG__ > 3
  #define LOG_DEBUG(msg) __LOG__("DEBG",msg)
#else
  #define LOG_DEBUG(msg)
#endif

#if __LEVEL_LOG__ > 2
  #define LOG_INFO(msg) __LOG__("INFO",msg)
#else
  #define LOG_INFO(msg)
#endif

#if __LEVEL_LOG__ > 1
  #define LOG_WARN(msg) __LOG__("WARN",msg)
  #define LOG_TEST(msg) __LOG__("TEST",msg)
#else
  #define LOG_WARN(msg)
  #define LOG_TEST(msg)
#endif

#if __LEVEL_LOG__ > 0
  #define LOG_ERROR(msg) __LOG__("FATL",msg)
#else
  #define LOG_ERROR(msg)
#endif

#if __LEVEL_ASSERT__ > 0
  #define ASSERT(exp, msg) if (!(exp)) { __LOG__("ASSERT FAILED", msg); assert(exp); }
#else
  #define ASSERT(exp,msg)
#endif

#if __PERF_STATS__ > 0
  #define INC_PERF_COUNTER(counter) counter++
  #define INC_PERF_COUNTER_BY(counter,amount) counter+=amount
  #define TAKE_MAX_PERF(counter,candidate) counter=counter<candidate?candidate:counter
#else
  #define INC_PERF_COUNTER(counter)
  #define INC_PERF_COUNTER_BY(counter,amount)
  #define TAKE_MAX_PERF(counter,candidate)
#endif

// We expect collection to have begin and end methods so that we can iterate through it.
template<class Collection>
std::string printCol(const Collection& iList) {
  std::ostringstream aOs("[ ");
  for (auto it = iList.begin(); it != iList.end(); ++it)
    aOs << (*it) << " ";
  aOs << "]";
  return aOs.str();
}

#endif /* LOGGING_H_ */
