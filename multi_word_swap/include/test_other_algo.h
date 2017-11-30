#pragma once

#include "test_common.h"

void test_mutex_swap_read_single();
void test_mutex_swap_write_single();
void test_mutex_swap_mt_read_unblock();
void test_mutex_swap_mt_write_unblock();
void test_mutex_swap_mt_many_readers();
void test_mutex_swap_mt_many_writers();
void test_mutex_swap_mt_many_readers_writers();

void test_spin_swap_read_single();
void test_spin_swap_write_single();
void test_spin_swap_mt_read_unblock();
void test_spin_swap_mt_write_unblock();
void test_spin_swap_mt_many_readers();
void test_spin_swap_mt_many_writers();
void test_spin_swap_mt_many_readers_writers();

void test_rwlock_swap_read_single();
void test_rwlock_swap_write_single();
void test_rwlock_swap_mt_read_unblock();
void test_rwlock_swap_mt_write_unblock();
void test_rwlock_swap_mt_many_readers();
void test_rwlock_swap_mt_many_writers();
void test_rwlock_swap_mt_many_readers_writers();

//////////////////////////////////////////////////////////////////////////////////////////////

static void test_all_mutex_swap() {
  test_mutex_swap_read_single();
  test_mutex_swap_write_single();
  test_mutex_swap_mt_read_unblock();
  test_mutex_swap_mt_write_unblock();
  test_mutex_swap_mt_many_readers();
  test_mutex_swap_mt_many_writers();
  test_mutex_swap_mt_many_readers_writers();
}

static void test_all_spin_swap() {
  test_spin_swap_read_single();
  test_spin_swap_write_single();
  test_spin_swap_mt_read_unblock();
  test_spin_swap_mt_write_unblock();
  test_spin_swap_mt_many_readers();
  test_spin_swap_mt_many_writers();
  test_spin_swap_mt_many_readers_writers();
}

static void test_all_rwlock_swap() {
  test_rwlock_swap_read_single();
  test_rwlock_swap_write_single();
  test_rwlock_swap_mt_read_unblock();
  test_rwlock_swap_mt_write_unblock();
  test_rwlock_swap_mt_many_readers();
  test_rwlock_swap_mt_many_writers();
  test_rwlock_swap_mt_many_readers_writers();
}

static void main_other_algo_unit_test() {
  test_all_mutex_swap();
  test_all_spin_swap();
  test_all_rwlock_swap();
}

