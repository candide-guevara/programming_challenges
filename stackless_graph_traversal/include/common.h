#pragma once

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <strings.h>
#include <string.h>

#include <logger.h>

#ifndef __LEVEL_ASSERT__
  #define __LEVEL_ASSERT__ 1
#endif

#if __LEVEL_ASSERT__ > 0
  #define ASSERT(exp, msg, ...) \
    if (!(exp)) { __LOG__("ASSERT", msg " : " #exp ,##__VA_ARGS__); __my_assert__(!!(exp)); }
#else
  #define ASSERT(exp,msg, ...)
#endif

#define TEST_ASSERT(exp, msg, ...) \
  if (!(exp)) { __LOG__("TEST_ASSERT", msg " : " #exp ,##__VA_ARGS__); __my_assert__(!!(exp)); }

#define STATIC_ASSERT(COND,MSG) \
  typedef char __static_assert_##MSG[(COND)?1:-1]

#pragma GCC diagnostic ignored "-Wunused-function"
// Use this to set breakpoints to trap assertion violations
static void __my_assert__(int condition) {
  assert(condition);
}
#pragma GCC diagnostic pop

