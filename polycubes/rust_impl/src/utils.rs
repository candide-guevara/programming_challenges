use std::collections::BinaryHeap;

use super::bloom::*;
use super::constants::*;

// Representation of IdxT when each coordinate is separated into its own variable.
pub type PointT = [IdxT; DIMS];

// An array of cells describing the individual cells of a polycube.
pub type PolyCubeT = [IdxT; CUBE_ARR_LEN];

pub trait Zeros { fn zeros() -> Self; }
impl<T,const N: usize> Zeros for [T;N] where T:Copy+Default {
  fn zeros() -> Self { return [T::default(); N]; }
}

// Use only for testing
pub fn build_cube(points:&[PointT]) -> PolyCubeT {
  let mut pc = PolyCubeT::zeros();
  for (i,p) in points.iter().enumerate() { pc[i] = point_to_idx(*p); }
  return pc;
}

// Use only for testing
pub fn to_points(pc:&[IdxT], size:usize) -> Vec::<PointT> {
  return pc.iter().take(size).map(|c| idx_to_point(*c)).collect::<Vec::<PointT>>();
}

// Use only for testing
pub fn vec_to_points(cubes:&Vec<PolyCubeT>, size:usize) -> Vec::<Vec::<PointT>> {
  let mut v:Vec::<Vec::<PointT>> = cubes.iter().map(|c| to_points(c, size)).collect();
  v.sort();
  return v;
}

pub fn idx_to_point(idx: IdxT) -> PointT {
  debug_assert!((idx as usize) < IDX_MAX);
  let mut p:PointT = [idx; DIMS];
  for (c, s) in p.iter_mut().zip(SHIFTS.iter()) {
    *c = (*c >> s) & MASK_X;
  }
  return p;
}
#[test]
fn idx_to_point_test() {
  let idx:IdxT = (1 << SHIFTS[0]) + (2 << SHIFTS[1]) + (3 << SHIFTS[2]);
  let p = idx_to_point(idx);
  assert_eq!(p, [1,2,3]);
}

pub fn point_to_idx(p: PointT) -> IdxT {
  let mut idx:IdxT = 0;
  for (c, s) in p.iter().zip(SHIFTS.iter()) {
    debug_assert!(*c <= MAX_COORD);
    idx += (c << s);
  }
  debug_assert!((idx as usize) < IDX_MAX);
  return idx;
}
#[test]
fn point_to_idx_test() {
  let p:PointT = [1,2,3];
  let idx = point_to_idx(p);
  assert_eq!(idx, (1 << SHIFTS[0]) + (2 << SHIFTS[1]) + (3 << SHIFTS[2]));
}

pub fn shift_by_vec_in_place(cubes: &mut[PolyCubeT], size:usize, offsets:&[IdxT]) {
  for (offset, cube) in offsets.iter().zip(cubes.iter_mut()) {
    for c in &mut cube[0..size] {
      let c0 = *c;
      *c =  (c0 + (offset & MASK_X)) & MASK_X;
      *c += (c0 + (offset & MASK_Y)) & MASK_Y;
      *c += (c0 + (offset & MASK_Z)) & MASK_Z;
    }
  }
}
#[test]
fn shift_by_vec_in_place_test() {
  let cube = build_cube(&[[1,2,3], [4,5,6], [1,2,3]]);
  let mut cubes = [cube, cube];
  let offsets = [point_to_idx([1,0,2]), point_to_idx([MAX_COORD,MAX_COORD-1,0])];
  shift_by_vec_in_place(&mut cubes[..], 2, &offsets);
  assert_eq!(to_points(&cubes[0], 3), vec![[2,2,5], [5,5,8], [1,2,3]]);
  assert_eq!(to_points(&cubes[1], 3), vec![[0,0,3], [3,3,6], [1,2,3]]);
}

// By how much a polycube must be shifted so that it is as close to the origin
// (0,0,0) as possible.
#[inline]
fn find_origin_offset(cube: &[IdxT]) -> IdxT {
  let [mut x, mut y,mut z] = MASKS;
  for c in cube {
    x = if (c & MASK_X) < x { c & MASK_X } else { x };
    y = if (c & MASK_Y) < y { c & MASK_Y } else { y };
    z = if (c & MASK_Z) < z { c & MASK_Z } else { z };
  }
  return  (((1 << 1*COORD_BIT_LEN) - x) & MASK_X)
        + (((1 << 2*COORD_BIT_LEN) - y) & MASK_Y)
        + (((1 << 3*COORD_BIT_LEN) - z) & MASK_Z);
}

pub fn shift_to_origin_vec(cubes: &mut [PolyCubeT], size:usize) {
  let mut offsets:Vec::<IdxT> = vec![0; cubes.len()];
  for (i,cube) in cubes.iter_mut().enumerate() {
    offsets[i] = find_origin_offset(&cube[0..size]);
  }
  shift_by_vec_in_place(cubes, size, &offsets);
}
#[test]
fn shift_to_origin_vec_test() {
  let mut cubes = [
    build_cube(&[[1,5,3], [4,4,3], [1,2,3]]),
    build_cube(&[[4,2,6], [2,5,2], [1,2,3]]),
  ];
  shift_to_origin_vec(&mut cubes, 2);
  assert_eq!(to_points(&cubes[0], 3), vec![[0,1,0], [3,0,0], [1,2,3]]);
  assert_eq!(to_points(&cubes[1], 3), vec![[2,0,4], [0,3,0], [1,2,3]]);
}

// Foreach cube sort its cells in descending order.
// If a cube has any duplicated cell then it is discarted.
// Uses a best-effort filter to remove dupe polycubes.
// Returns the number of cubes written.
pub fn sort_cube_cells_and_dedupe(cubes: &mut [PolyCubeT], size:usize) -> usize {
  let mut bloom = Bloom::new();
  let mut heap = BinaryHeap::<IdxT>::with_capacity(CUBE_ARR_LEN);
  let mut write_i = 0;
  for read_i in 0..cubes.len() {
    let rcube = &cubes[read_i];
    heap.extend(&rcube[0..size]);

    let mut j = 0;
    let wcube = &mut cubes[write_i];
    while let Some(c) = heap.pop() {
      if j != 0 && wcube[j-1] == c { break; }
      wcube[j] = c;
      j += 1;
    }
    if j == size {
      if !bloom.contains(wcube) { write_i += 1; }
    }
    heap.clear();
  }
  debug_assert!(write_i > 0 && write_i <= cubes.len());
  return write_i;
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
  assert_eq!(to_points(&cubes[0], 3), vec![[1,5,3], [4,4,3], [1,2,3]]);
  assert_eq!(to_points(&cubes[1], 3), vec![[2,5,2], [4,2,1], [1,2,6]]);
}

