# Monkey CAS

Monkey cas is a multi-word compare-and-swap software (hopefully correct) implementation.

The only implementation of a multi-cas I could find was [this][0]. My goal is to create a preemption-immune and lock-free implementation with less use of atomic operations.
Of course there is a catch. My implementation can only support a limited (but configurable) number of readers and writers.

## Implementation

To illustrate the idea through an example, let's use a (x,y,z) coordinate tuple. Normally it would be represented like this :

    struct Coord {
        int x,y,z;
    };

With monkey-cas it will be written as follows :

    struct Control {
        unsigned int latest_slot:SLOT_EXP;
        unsigned int contending_slot:SLOT_EXP;
        unsigned int version_1_lock:LOCK_EXP;
        unsigned int version_2_lock:LOCK_EXP;
        unsigned int version_3_lock:LOCK_EXP;
        unsigned int version_4_lock:LOCK_EXP;
    };
    struct Coord {
        int x[SLOTS], y[SLOTS], z[SLOTS];
    };

`struct Control` must be accessed atomically so its size is limited and architecture dependent. 
The `LOCK` and `SLOT` parameters correspond to : 

* `SLOTS == 2 ^ SLOT_EXP` is the maximum number of lock-free writers
* `LOCKS == 2 ^ LOCK_EXP` is the maximum number of readers that can lock a given version
* Each slot has a corresponding `version_*_lock`, if non-zero, it cannot be modified by writers
* The configuration space must satisfy : `max_atomic_word_len >= 2*SLOT_EXP + 2^SLOT_EXP * LOCK_EXP`

Readers should execute the following sequence of instructions :

* 1. Load acquire `struct Control`
* 2. Increment the value of `version_*_lock` corresponding to `latest_slot`
  * If `version_*_lock == max_locks` the **reader must wait** 
* 3. Attempt a CAS (with release memory order) of `struct Control`. If CAS fails, starts again
* 4. Non-atomic read `x[latest_slot], y[latest_slot], z[latest_slot]`
* 5. Decrement `version_*_lock` of the read slot
* 6. Attempt a CAS (with relaxed memory order) of `struct Control`. If CAS fails, start from last step
  * You may need a load barrier to ensure the `x,y,z` reads happen before the unlock (otherwise use CAS with release memory order)

Writers should execute the following sequence of instructions :

* 1. Load acquire `struct Control`
* 2. Increment `contending_slot` so that it correspond to a non-locked slot and `contending_slot != latest_slot`
  * If increment goes over `latest_slot` the **writer must wait**
* 3. Attempt a CAS (with release memory order) of `struct Control`. If CAS fails, starts again
* 4. Non-atomic write `x[contending_slot], y[contending_slot], z[contending_slot]`
* 5. Set `latest_slot` to the written slot
* 6. Attempt a CAS (with relaxed memory order) of `struct Control`. If CAS fails, start from last step
  * You may need a load barrier to ensure the `x,y,z` writes happen before the unlock (otherwise use CAS with release memory order)
* 7. If CAS fails because of changes to the read locks, update their values and try again
* 8. If CAS fails because of changes in `contending_slot` or `latest_slot`, start from scratch
  * If `version_*_lock` for `contending_slot` has been taken, also start from scratch
  * Unfortunately at this point the writer is vulnerable to the **ABA problem**.  
    If the writer sleeps for long enough after securing a `contending_slot`, it may wake-up after several writes have completed. 
    The state of `latest_slot` and `contending_slot` may have returned to the same value as when it started.

## A tweaked implementation higher write concurrency

This is a trade-off to allow for more writers but less readers. It also suffers from the **ABA problem** if you really saturate writes.
Instead of having a bit field lock for each slot, we limit the number of slots that can be locked. `struct Control` then becomes :

    struct Control {
        unsigned int latest_slot:SLOT_EXP;
        unsigned int contending_slot:SLOT_EXP;
        unsigned int version_1_slot:SLOT_EXP;
        unsigned int version_1_lock:LOCK_EXP;
        unsigned int version_2_slot:SLOT_EXP;
        unsigned int version_2_lock:LOCK_EXP;
    };

The configuration space must satisfy : `max_atomic_word_len >= 2*SLOT_EXP + SLOT_COUNT * (SLOT_EXP + LOCK_EXP)`.
Example: you can have 4096 write slots and 2 version locks holding 256 writers each : `64 >= 12 * 2 + 2 * (12 + 8)`.
But at this point you may start noticing you are trashing your TLB at each atomic access ?

You may also modify some of the writer steps if you know you have far more slots than writers :

* 2. Do not wait if you go past `latest_slot`
* 8. Wait for a short while before starting from scratch. This should let other writers finish.

## ABA resistant implementation

The idea is to have a writer's bit mask covering each slot. This way we can prevent 2 writers contend on the same slot.
The configuration space must satisfy : `max_atomic_word_len >= SLOT_EXP + 2^SLOT_EXP + SLOT_COUNT(SLOT_EXP + LOCK_EXP)`

    struct Control {
        unsigned int latest_slot:SLOT_EXP;
        unsigned int writer_mask:SLOTS;
        unsigned int version_1_slot:SLOT_EXP;
        unsigned int version_1_lock:LOCK_EXP;
        unsigned int version_2_slot:SLOT_EXP;
        unsigned int version_2_lock:LOCK_EXP;
    };

The write sequence becomes :

* 2. Loop on the bits in `writer_mask` starting from the index `latest_slot + 1`. Set the first 0 bit found that does not correspond to a reader locked slot.
  * If increment goes over `latest_slot` the **writer must wait**
* 5. Set `latest_slot` to the written slot and clear the corresponding bit in `writer_mask`.
* 8. If CAS fails because of changes in `latest_slot`, start from scratch
  * Assert the bit for `latest_slot` in `writer_mask` is not set and there is no reader lock on that slot. Otherwise something went (terribly) wrong

[0]: https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-579.pdf

