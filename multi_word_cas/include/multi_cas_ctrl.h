#pragma once

#include <common.h>
#include <stdbool.h>
#include <stdint.h>
#ifdef __clang__
#include <stdatomic.h>
#endif

#define CACHE_LINE_SIZE 64
#define ALIGN_CACHE_LINE _Alignas(CACHE_LINE_SIZE)
#define ADD_CACHE_LINE_PADDING(sizeof_exp) char __padding[CACHE_LINE_SIZE - (sizeof_exp)];

#ifndef MAX_ACQUIRE_CAS_ATTEMPTS
#define MAX_ACQUIRE_CAS_ATTEMPTS 32
#endif
#ifndef MAX_RELEASE_CAS_ATTEMPTS
#define MAX_RELEASE_CAS_ATTEMPTS 64
#endif
#ifndef WAIT_QUANTUM
#define WAIT_QUANTUM 41
#endif
#ifndef MORE_ATO_LESS_CONTENTION
#define MORE_ATO_LESS_CONTENTION 1
#endif
#ifndef MEMORY_ORDER_REL_LOCK
#define MEMORY_ORDER_REL_LOCK memory_order_acq_rel
#endif
#ifndef MEMORY_ORDER_ACQ_LOCK
#define MEMORY_ORDER_ACQ_LOCK memory_order_acq_rel
#endif

#define NOABA_SLOT_EXP 4u
#define NOABA_READER_EXP 7u
#define NOABA_VERSION_COUNT 4u
#define NOABA_SLOTS (1llu << NOABA_SLOT_EXP)
#define NOABA_ALL_WRITERS ((1llu << NOABA_SLOTS) - 1)
#define NOABA_WRITER_MASK (NOABA_ALL_WRITERS << NOABA_SLOT_EXP)
#define NOABA_LAST_SLOT (NOABA_SLOTS - 1)
#define NOABA_READERS (1llu << NOABA_READER_EXP)
#define NOABA_LAST_READER (NOABA_READERS - 1)
#define NOABA_READER_HIGH_MARK (1llu << (NOABA_READER_EXP-1))
#define NOABA_READER_LAST_MARK (NOABA_READER_HIGH_MARK - 1)

//////////////////////////////////////////////////////////////////////////////////////////////

typedef void (*cas_content_writer)(void *restrict, uint32_t, void *restrict);
typedef void (*cas_content_reader)(void *restrict, uint32_t, void *restrict);

//////////////////////////////////////////////////////////////////////////////////////////////

typedef struct MCas_NoABA MCas_NoABA;
typedef union CtrlView_NoABA CtrlView_NoABA;

union CtrlView_NoABA {
#ifdef __clang__
  atomic_uintmax_t raw;
#else
  uintmax_t raw;
#endif
  uintmax_t _raw;
  struct {
    uintmax_t latest_slot : NOABA_SLOT_EXP;
    uintmax_t writer_mask : NOABA_SLOTS;
    /*
    uint8_t version_1_slot:NOABA_SLOT_EXP;
    uint8_t version_1_lock:NOABA_READER_EXP;
     ...
    */
    uintmax_t version_locks : NOABA_VERSION_COUNT *(NOABA_SLOT_EXP + NOABA_READER_EXP);
  };
};

struct MCas_NoABA {
  CtrlView_NoABA control;
  void *content;
#ifdef __clang__
  ADD_CACHE_LINE_PADDING(sizeof(atomic_uintmax_t) + sizeof(void *))
#else
  ADD_CACHE_LINE_PADDING(sizeof(uintmax_t) + sizeof(void *))
#endif
};

//////////////////////////////////////////////////////////////////////////////////////////////

typedef struct LockDesc LockDesc;
typedef struct MCAS_Stat MCAS_Stat;

struct LockDesc {
  uint16_t slot;
  uint16_t lock;
  uint16_t version;
};

struct MCAS_Stat {
  uint32_t acquire_tries;
  uint32_t release_tries;
  uint32_t wait_spins;
  uint32_t version_overflow;
  uint32_t all_lock_taken;
  uint32_t to_drain;
  uint32_t success;
};

//////////////////////////////////////////////////////////////////////////////////////////////

STATIC_ASSERT(sizeof(uintmax_t) == sizeof(uint64_t), check_atomic_ops_for_this_width);
#ifdef __clang__
STATIC_ASSERT(sizeof(atomic_uintmax_t) == sizeof(uintmax_t), atomic_uintmax_too_big);
#endif
STATIC_ASSERT(sizeof(CtrlView_NoABA) == sizeof(uintmax_t), control_too_big_noaba);
STATIC_ASSERT(sizeof(LockDesc) <= sizeof(uint64_t), lock_desc_bad_size);

//////////////////////////////////////////////////////////////////////////////////////////////

