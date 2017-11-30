# Monkey Swap

Monkey swap is a multi-word atomic swap software implementation (hopefully roughly correct).
My goal is to create a preemption-immune and lock-free implementation with a minimum use of atomic operations.
My implementation can only support a limited (but configurable) number of readers and writers, and is far from optimal.

This all started when I was looking at the possibilities for a multi word compare and swap.
The only implementation of a multi-cas I could find was [this][0].

## Implementations

To illustrate the idea through an example, let's use a (x,y,z) coordinate tuple. Normally it would be represented like this :

    struct Coord {
        int x,y,z;
    };

With monkey-swap it will be written as follows we will add a `struct Control` to mediate the access between writers and readers.
`struct Control` must be accessed atomically so its size is limited and architecture dependent. 
`Coord` will also be duplicated to allow several writes in parallel.

    struct Coord {
        int x[SLOTS], y[SLOTS], z[SLOTS];
    };

### Optimizing for a lot of read handles : Ctrl_Count

With monkey-swap it will be written as follows :

    struct Ctrl_Count {
        uintmax_t latest_slot:SLOT_EXP;
        uintmax_t writer_mask:SLOTS;
        uintmax_t version_1_slot:SLOT_EXP;
        uintmax_t version_1_lock:LOCK_EXP;
        uintmax_t version_2_slot:SLOT_EXP;
        uintmax_t version_2_lock:LOCK_EXP;
    };

The `LOCK` and `SLOT` parameters correspond to : 

* `SLOTS == 2 ^ SLOT_EXP` is the maximum number of lock-free writers
* `LOCKS == 2 ^ LOCK_EXP` is the maximum number of readers that can lock a given version
* The configuration space must satisfy : `max_atomic_word_len >= SLOT_EXP + 2^SLOT_EXP + VERSION_COUNT * (SLOT_EXP + LOCK_EXP)` 

The idea is to have a writer's bit mask covering each slot. This way we can prevent 2 writers contend on the same slot.
However there are fewer reader locks than slots.

Readers should execute the following sequence of instructions :

* 1. Load acquire `struct Control`
* 2. Increment the value of `version_*_lock` corresponding to `latest_slot`
  * If `version_*_lock == max_locks` the **reader must wait** 
  * If there is no `version_*_lock` for `latest_slot` but a `version_*_lock` is free, allocate it to `latest_slot`
  * If there is no `version_*_lock` for `latest_slot` and all `version_*_lock` are taken the **reader must wait**
* 3. Attempt a CAS (with release memory order) of `struct Control`. If CAS fails, starts again
* 4. Non-atomic read `x[latest_slot], y[latest_slot], z[latest_slot]`
* 5. Decrement `version_*_lock` of the read slot
* 6. Attempt a CAS (with relaxed memory order) of `struct Control`. If CAS fails, start from last step
  * You may need a load barrier to ensure the `x,y,z` reads happen before the unlock (otherwise use CAS with release memory order)
  * Even if the value of `struct Control` changed it should never reach zero

Writers should execute the following sequence of instructions :

* 1. Load acquire `struct Control`
* 2. Pick 1 slot that is not already set in `writer_mask` **and** is not the `latest_slot` **or** is blocked by a reader lock. Set that slot bit in `writer_mask`.
  * If no free slot available **writer must wait**
* 3. Attempt a CAS (with release memory order) of `struct Control`. If CAS fails, starts again
* 4. Non-atomic write `x[contending_slot], y[contending_slot], z[contending_slot]`
* 5. Set `latest_slot` to the written slot and clear the corresponding bit in `writer_mask`.
* 6. Attempt a CAS (with relaxed memory order) of `struct Control`. If CAS fails, start from last step
  * You may need a load barrier to ensure the `x,y,z` writes happen before the unlock (otherwise use CAS with release memory order)

This implementation is however vulnerable to preemption. If there are enough writers to lock all write slots and one of them is prempted during the writing sequence, it may deadlock.
Also the CAS operations do not guarantee forward progress, in cases of high contention we might end up spinning endlessly in the CAS loops.

### Optimizing for better forward progress : Ctrl_Bitfd

If instead of counting the number of writers and the latest slot, we use bit fields to represent them, we can use atomic fetch_or fetch_and to modify them without the need for a CAS loop.

    struct Ctrl_Bitfd {
        uintmax_t latest_slot:SLOTS;
        uintmax_t writer_mask:SLOTS;
        uintmax_t version_1_slot:SLOT_EXP;
        uintmax_t version_1_lock:LOCKS;
        uintmax_t version_2_slot:SLOT_EXP;
        uintmax_t version_2_lock:LOCKS;
    };

In this case the release operation for the reader becomes :

* 5. Clear reader bit from its corresponding `version_*_lock`
* 6. Nothing !

As for the writer :

* 5. Clear the writer bit from `writer_mask` and set the `latest_slot` bit of the writer's slot
* 6. Nothing !

This implementation has a shorter release critical section but can accomodate fewer concurrent readers. 

### Using the OS facilities in case of high contention

If too many readers encounter full `version_*_lock`, we need a way to queue them fairly to lessen the rate of atomic changes to `struct Control` and provide some fairness.
The pattern is to yield the thread to the OS in case writers or readers cannot progress (I got this idea [from here][1]).

## Wait, why not just use RCU ?

We could just store `Coord` in a `void*`, each writer will atomically swap that pointer, each reader will atomically read it.
This is much more simpler and faster. I hope there are however some advantages.

* Constant memory usage : you do not have to rely in an out-of-bound garbage collection/harzard pointers/grace period
* And that is about it, at the end this is pretty silly ...

Hopefully it will be at least a good exercise to use C11 atomics.

[0]: https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-579.pdf
[1]: http://preshing.com/20150316/semaphores-are-surprisingly-versatile/

