#pragma once

#include <common.h>
#include <stdbool.h>
#include <stdint.h>
#ifdef __clang__
#include <stdatomic.h>
#endif

typedef void (*cas_content_writer)(void *, uint32_t, void *);
typedef void (*cas_content_reader)(void *, uint32_t, void *);

typedef struct Control_NoABA Control_NoABA;
typedef struct Control_HeavyWrite Control_HeavyWrite;
typedef struct Control_Vanilla Control_Vanilla;

#define NOABA_SLOT_EXP 4
#define NOABA_READER_EXP 7
#define NOABA_VERSION_COUNT 4
#define NOABA_SLOTS (1 << NOABA_SLOT_EXP)
#define NOABA_READERS (1 << NOABA_READER_EXP)

struct Control_NoABA {
  uintmax_t latest_slot : NOABA_SLOT_EXP;
  uintmax_t writer_mask : NOABA_SLOTS;
  /*
  uint8_t version_1_slot:NOABA_SLOT_EXP;
  uint8_t version_1_lock:NOABA_READER_EXP;
   ...
  */
  uintmax_t version_locks : NOABA_VERSION_COUNT *(NOABA_SLOT_EXP + NOABA_READER_EXP);
};

#define VANILLA_SLOT_EXP 4
#define VANILLA_LOCK_EXP 3
#define VANILLA_SLOTS (1 << VANILLA_SLOT_EXP)
#define VANILLA_LOCKS (1 << VANILLA_LOCK_EXP)
#define VANILLA_VERSION_COUNT VANILLA_SLOTS

struct Control_Vanilla {
  uint8_t latest_slot : VANILLA_SLOT_EXP;
  uint8_t contending_slot : VANILLA_SLOT_EXP;
  /*
  uint8_t version_1_lock:VANILLA_LOCK_EXP;
   ...
  */
  uintmax_t version_locks : VANILLA_LOCKS *VANILLA_LOCK_EXP;
};

#define HWRITE_SLOT_EXP 10
#define HWRITE_LOCK_EXP 10
#define HWRITE_VERSION_COUNT 2
#define HWRITE_SLOTS (1 << HWRITE_SLOT_EXP)
#define HWRITE_LOCKS (1 << HWRITE_LOCK_EXP)

struct Control_HeavyWrite {
  uintmax_t latest_slot : HWRITE_SLOT_EXP;
  uintmax_t contending_slot : HWRITE_SLOT_EXP;
  /*
  uint8_t version_1_slot:HWRITE_SLOT_EXP;
  uint8_t version_1_lock:HWRITE_LOCK_EXP;
   ...
  */
  uintmax_t version_locks : HWRITE_VERSION_COUNT *(HWRITE_SLOT_EXP + HWRITE_LOCK_EXP);
};

//////////////////////////////////////////////////////////////////////////////////////////////

typedef union CtrlView_NoABA CtrlView_NoABA;
typedef union CtrlView_HeavyWrite CtrlView_HeavyWrite;
typedef union CtrlView_Vanilla CtrlView_Vanilla;

union CtrlView_NoABA {
#ifdef __clang__
  atomic_uintmax_t raw;
#else
  uintmax_t raw;
#endif
  Control_NoABA fields;
};

union CtrlView_HeavyWrite {
  uintmax_t raw;
  Control_HeavyWrite fields;
};

union CtrlView_Vanilla {
  uintmax_t raw;
  Control_Vanilla fields;
};

//////////////////////////////////////////////////////////////////////////////////////////////

typedef struct MCas_NoABA MCas_NoABA;
typedef struct MCas_HeavyWrite MCas_HeavyWrite;
typedef struct MCas_Vanilla MCas_Vanilla;

struct MCas_NoABA {
  CtrlView_NoABA control;
  void *content;
};

struct MCas_HeavyWrite {
  CtrlView_HeavyWrite control;
  void *content;
};

struct MCas_Vanilla {
  CtrlView_Vanilla control;
  void *content;
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
  uint32_t all_ver_taken;
  bool success;
};

//////////////////////////////////////////////////////////////////////////////////////////////

STATIC_ASSERT(sizeof(uintmax_t) == sizeof(uint64_t), check_atomic_ops_for_this_width);

STATIC_ASSERT(sizeof(CtrlView_NoABA) == sizeof(uintmax_t), control_too_big_noaba);
STATIC_ASSERT(sizeof(CtrlView_HeavyWrite) == sizeof(uintmax_t), control_too_big_heavywrite);
STATIC_ASSERT(sizeof(CtrlView_Vanilla) == sizeof(uintmax_t), control_too_big_vanilla);
STATIC_ASSERT(sizeof(LockDesc) <= sizeof(uint64_t), lock_desc_bad_size);
