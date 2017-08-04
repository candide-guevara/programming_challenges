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
  #define ASSERT(exp, msg) \
    if (!(exp)) { __LOG__("ASSERT", msg " : " #exp ); assert(exp); }
#else
  #define ASSERT(exp,msg)
#endif

#define __STRINGIFY__(s) #s
#define __PASTE_STRINGS__(a,b) a ## b

#define STRINGIFY(s) __STRINGIFY__(s)
#define PASTE_STRINGS(a,b) __PASTE_STRINGS__(a,b)


