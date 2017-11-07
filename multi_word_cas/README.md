# Monkey CAS

Monkey cas is a multi-word compare-and-swap software (hopefully roughly correct) implementation.

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
  * Even if the value of `struct Control` changed it should never reach zero

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

## ABA resistant implementation

The idea is to have a writer's bit mask covering each slot. This way we can prevent 2 writers contend on the same slot.
The configuration space must satisfy : `max_atomic_word_len >= SLOT_EXP + 2^SLOT_EXP + VERSION_COUNT * (SLOT_EXP + LOCK_EXP)`

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
* 5. Set `latest_slot` to the written slot and clear the corresponding bit in `writer_mask`.
* 8. If CAS fails because of changes in `latest_slot`, start from scratch
  * Assert `latest_slot != writer_slot` and there is no version lock on the writer slot. Otherwise something went (terribly) wrong

This implementation is however vulnerable to preemption. If there are enough writers to lock all write slots and one of them is prempted during the writing sequence, it may deadlock.

## A tweaked implementation higher write concurrency

This is a trade-off to allow for more writers but less readers. 
It can be made inmune to ABA and premption by assign each thread an exclusive pool of slots.
Instead of having a bit field lock for each slot, we limit the number of slots that can be locked. `struct Control` then becomes :

    struct Control {
        unsigned int latest_slot:SLOT_EXP;
        unsigned int contending_slot:SLOT_EXP;
        unsigned int version_1_slot:SLOT_EXP;
        unsigned int version_1_lock:LOCK_EXP;
        unsigned int version_2_slot:SLOT_EXP;
        unsigned int version_2_lock:LOCK_EXP;
    };

The configuration space must satisfy : `max_atomic_word_len >= 2*SLOT_EXP + VERSION_COUNT * (SLOT_EXP + LOCK_EXP)`.
Example: you can have 4096 write slots and 2 version locks holding 256 writers each : `64 >= 12 * 2 + 2 * (12 + 8)`.
But at this point you may start noticing you are trashing your TLB at each atomic access ?

The write sequence becomes :

* 2. Set `contending_slot` to the first slot in this thread slot pool that is not locked by a reader and is not the `latest_slot`
  * If all pool slots are taken the **writer must wait**
* 8. Wait for a short while before starting from scratch. This should let other writers finish.

## Fixing premption with per-cpu clocks

Another way to fix the ABA and premption problem is to use per-cpu clocks and have only 1 slot per cpu.
The write sequence becomes :

* 0. Non-atomic increment per-cpu clock and remember value (this operation cannot be reordered)
* 2. Increment `contending_slot` so that it correspond to a non-locked slot and `contending_slot != latest_slot` (loop as many times over the slot space as needed)
* 4. Non-atomic write `x[contending_slot], y[contending_slot], z[contending_slot]`
  * Between each write check the per-cpu clock has not moved (interspeeding of writing and reading should not be reordered)
  * If clock changes in the middle of a write, start from scratch


## Wait, why not just use RCU ?

We could just store `Coord` in a `void*`, each writer will atomically swap that pointer, each reader will atomically read it.
This is much more simpler and faster. I hope there are however some advantages.

* Constant memory usage : you do not have to rely in an out-of-bound garbage collection/harzard pointers/grace period
* And that is about it, at the end this is pretty silly ...

Hopefully it will be at least a good exercise to use C11 atomics.

[0]: https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-579.pdf

