use rustc_hash::*;
use std::collections::BinaryHeap;

use super::constants::*;
use super::bloom::*;
use super::binary_heap::*;
use super::utils::*;

// Foreach cube sort its cells in descending order.
// If a cube has any duplicated cell then it is discarted.
// Uses a best-effort filter to remove dupe polycubes.
// Returns the number of cubes written.
pub fn sort_cube_cells_and_dedupe(cubes: &mut [PolyCubeT], size:usize) -> usize {
  #[cfg(feature="use_my_heap")]
  return sort_cube_cells_and_dedupe_my_heap(cubes, size);
  #[cfg(feature="use_bin_heap")]
  return sort_cube_cells_and_dedupe_bin_heap(cubes, size);
  #[cfg(feature="use_sort_and_hash")]
  return sort_cube_cells_and_dedupe_sort_and_hash(cubes, size);
  #[cfg(feature="use_double_sort")]
  return sort_cube_cells_and_dedupe_double_sort(cubes, size);
}

fn sort_cube_cells_and_dedupe_my_heap(cubes: &mut [PolyCubeT], size:usize) -> usize {
  let mut bloom = Bloom::default();
  let mut heap = BinHeap::new();
  let mut write_i = 0;
  'outer: for read_i in 0..cubes.len() {
    let rcube = &cubes[read_i];
    heap.clear();
    for c in &rcube[0..size] { heap.push(*c); }

    let wcube = &mut cubes[write_i];
    for k in 0..size {
      wcube[k] = heap.pop();
      if k != 0 && wcube[k-1] == wcube[k] { continue 'outer; }
    }
    if bloom.insert(wcube) { write_i += 1; }
  }
  debug_assert!(write_i > 0 && write_i <= cubes.len());
  return write_i;
}

fn sort_cube_cells_and_dedupe_bin_heap(cubes: &mut [PolyCubeT], size:usize) -> usize {
  let mut bloom = Bloom::default();
  let mut heap = BinaryHeap::<IdxT>::with_capacity(CUBE_ARR_LEN);
  let mut write_i = 0;
  'outer: for read_i in 0..cubes.len() {
    let rcube = &cubes[read_i];
    heap.clear();
    heap.extend(&rcube[0..size]);

    let mut j = 0;
    let wcube = &mut cubes[write_i];
    while let Some(c) = heap.pop() {
      if j != 0 && wcube[j-1] == c { continue 'outer; }
      wcube[j] = c;
      j += 1;
    }
    if bloom.insert(wcube) { write_i += 1; }
  }
  debug_assert!(write_i > 0 && write_i <= cubes.len());
  return write_i;
}

fn sort_cube_cells_and_dedupe_sort_and_hash(cubes: &mut [PolyCubeT], size:usize) -> usize {
  let mut filter = FxHashSet::<PolyCubeT>::default();
  let mut write_i = 0;
  'outer: for read_i in 0..cubes.len() {
    let rcube = &mut cubes[read_i];
    //rcube[0..size].sort();
    rcube[0..size].sort_by(|a, b| b.cmp(a));

    for j in 0..size-1 {
      if rcube[j] == rcube[j+1] { continue 'outer; }
    }
    cubes[write_i] = cubes[read_i];
    if filter.insert(cubes[read_i]) { write_i += 1; }
  }
  debug_assert!(write_i > 0 && write_i <= cubes.len());
  return write_i;
}

fn sort_cube_cells_and_dedupe_double_sort(cubes: &mut [PolyCubeT], size:usize) -> usize {
  let mut write_i = 0;
  'outer: for read_i in 0..cubes.len() {
    let rcube = &mut cubes[read_i];
    rcube[0..size].sort_by(|a, b| b.cmp(a));

    for j in 0..size-1 {
      if rcube[j] == rcube[j+1] { continue 'outer; }
    }
    cubes[write_i] = cubes[read_i];
    write_i += 1;
  }
  cubes[0..write_i].sort();
  let mut final_i = 1;
  for read_i in 0..write_i-1 {
    if cubes[read_i][0..size] == cubes[read_i+1][0..size] { continue; }
    cubes[final_i] = cubes[read_i+1];
    final_i += 1;
  }
  debug_assert!(final_i <= cubes.len());
  return final_i;
}

#[test]
fn sort_cube_cells_and_dedupe_test() {
  let mut cubes = [
    build_cube(&[[1,5,3], [4,4,3], [1,2,3]]),
    build_cube(&[[1,5,3], [1,5,3], [1,2,6]]),
    build_cube(&[[4,2,1], [2,5,2], [1,2,4]]),
  ];
  let uniq_count = sort_cube_cells_and_dedupe(&mut cubes, 2);
  assert_eq!(uniq_count, 2);
  #[cfg(not(feature="use_double_sort"))] {
    assert_eq!(to_points(&cubes[0], 2), vec![[1,5,3], [4,4,3]]);
    assert_eq!(to_points(&cubes[1], 2), vec![[2,5,2], [4,2,1]]);
  }
  #[cfg(feature="use_double_sort")] {
    assert_eq!(to_points(&cubes[1], 2), vec![[1,5,3], [4,4,3]]);
    assert_eq!(to_points(&cubes[0], 2), vec![[2,5,2], [4,2,1]]);
  }
}

#[test]
fn sort_cube_cells_and_dedupe_double_sort_test() {
  let mut cubes = [
    build_cube(&[[1,5,3], [4,4,3], [1,2,3]]),
    build_cube(&[[4,2,1], [2,5,2], [1,2,4]]),
    build_cube(&[[1,5,3], [4,4,3], [1,2,6]]),
    build_cube(&[[1,5,3], [4,4,3], [1,2,6]]),
    build_cube(&[[4,2,1], [2,5,2], [1,2,4]]),
    build_cube(&[[0,0,1], [0,0,0], [1,2,4]]),
    build_cube(&[[0,0,1], [0,0,0], [1,2,4]]),
  ];
  let uniq_count = sort_cube_cells_and_dedupe_double_sort(&mut cubes, 2);
  assert_eq!(uniq_count, 3);
}

