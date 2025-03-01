mod bloom;
mod constants;
mod linear_alg;
mod precomputation;
mod utils;

use std::collections::HashSet;
use std::slice;
use std::time;

use constants::*;
use precomputation::*;
use utils::*;

// Scratch space needed to compute candidates and dedupe them.
// Re-used to minimize allocations.
struct ScratchSpace {
  offsets: Vec<AdjOffsetT>,
  candidates: Vec<PolyCubeT>,
  rot_candidates: Vec<PolyCubeT>,
  candidate_filter: HashSet<PolyCubeT>,
  new_cubes: Vec<PolyCubeT>,
  all_cubes: Vec<PolyCubeT>,
}

impl ScratchSpace {
  fn fill_candidates_with(&mut self, seed: &PolyCubeT, size: usize) {
    self.candidates.clear();
    self.candidates.resize(2*DIMS*size, *seed);
  }
  fn select_offsets(&mut self, seed: &PolyCubeT, size: usize, precomp: &Precomputed) {
    self.offsets.clear();
    self.offsets.resize(size, AdjOffsetT::zeros());
    //for (c,o) in (&seed[0..size]).iter().zip(self.offsets.iter_mut()) {
    //  *o = precomp.adj_offsets[*c as usize];
    for j in 0..size {
      self.offsets[j] = precomp.adj_offsets[seed[j] as usize];
    }
  }
  fn select_rotations(&mut self, seed: &PolyCubeT, size: usize, precomp: &Precomputed) {
    self.rot_candidates.clear();
    self.rot_candidates.resize(POSSIBLE_ROTATIONS, PolyCubeT::zeros());
    for j in 0..size {
      for r in 0..POSSIBLE_ROTATIONS {
        self.rot_candidates[r][j] = precomp.rot_cells[seed[j] as usize][r];
      }
    }
  }
  fn append_adj_to_cubes(&mut self, size: usize, precomp: &Precomputed) {
    for i in 0..size {
      let adj_i = self.candidates[0][i] as usize;
      for j in 0..(2*DIMS) {
        self.candidates[2*DIMS*i+j][size] = precomp.adj_cells[adj_i][j];
      }
    }
  }
  fn new() -> ScratchSpace {
    return ScratchSpace {
      offsets: Vec::<AdjOffsetT>::with_capacity(MAX_SIZE),
      candidates: Vec::<PolyCubeT>::with_capacity(2*DIMS*MAX_SIZE),
      rot_candidates: Vec::<PolyCubeT>::with_capacity(POSSIBLE_ROTATIONS),
      candidate_filter: HashSet::<PolyCubeT>::with_capacity(IDX_MAX * MAX_SIZE),
      new_cubes: Vec::<PolyCubeT>::with_capacity(2*DIMS*MAX_SIZE),
      all_cubes: Vec::<PolyCubeT>::with_capacity(2usize.pow(64)),
    };
  }
}

// From `seed` derive all possible polycubes of `size+1` which result from adding an adjacent cell.
fn propose_candidates(seed: &PolyCubeT, size:usize, precomp: &Precomputed, scratch:&mut ScratchSpace) {
  scratch.fill_candidates_with(seed, size);
  scratch.select_offsets(seed, size, precomp);
  scratch.append_adj_to_cubes(size, precomp);
  let offsets = unsafe {
    slice::from_raw_parts(scratch.offsets.as_ptr() as *const IdxT,
                          2 * DIMS * scratch.offsets.len())
  };
  shift_by_vec_in_place(&mut scratch.candidates, size, offsets);
  let candidate_len = sort_cube_cells_and_dedupe(&mut scratch.candidates, size+1);
  scratch.candidates.truncate(candidate_len);
}
#[test]
fn propose_candidates_test() {
  let seed = build_cube(&[[1,0,0], [0,0,0]]);
  let precomp = precomputation_helper(point_to_idx([3,3,3]) as usize);
  let mut scratch = ScratchSpace::new();
  propose_candidates(&seed, 2, &precomp, &mut scratch);
  let mut expect = vec![
    [[2,0,0], [1,0,0], [0,0,0]],
    [[0,1,0], [1,0,0], [0,0,0]],
    [[1,1,0], [1,0,0], [0,0,0]],
    [[1,1,0], [0,1,0], [0,0,0]],
    [[1,1,0], [0,1,0], [1,0,0]],
    [[0,0,1], [1,0,0], [0,0,0]],
    [[1,0,1], [1,0,0], [0,0,0]],
    [[1,0,1], [0,0,1], [0,0,0]],
    [[1,0,1], [0,0,1], [1,0,0]],
  ];
  expect.sort();
  assert_eq!(vec_to_points(&scratch.candidates, 3), expect);
}

// For each `self.candidates` computes all possible rotations.
// Add each rotated polycube to the set of previously seen polycubes.
// If no rotated version was ever seen before, this is a new polycube.
fn filter_candidates(candidates: &[PolyCubeT], size: usize, precomp: &Precomputed, scratch:&mut ScratchSpace) {
  scratch.new_cubes.clear();
  for candidate in candidates {
    scratch.select_rotations(&candidate, size, precomp);
    shift_to_origin_vec(&mut scratch.rot_candidates, size);
    let rot_len = sort_cube_cells_and_dedupe(&mut scratch.rot_candidates, size);
    scratch.rot_candidates.truncate(rot_len);

    let mut dupes = 0;
    for rot_candidate in &scratch.rot_candidates {
      if !scratch.candidate_filter.insert(*rot_candidate) { dupes += 1; }
    }
    if dupes == 0 { scratch.new_cubes.push(*candidate); }
  }
}
#[test]
fn filter_candidates_test() {
  let seeds = [
    build_cube(&[[1,1,0], [1,0,0], [0,0,0]]),
    build_cube(&[[2,0,0], [1,0,0], [0,0,0]]),
    build_cube(&[[1,1,0], [0,1,0], [0,0,0]]),
  ];
  let precomp = precomputation_helper(point_to_idx([3,3,3]) as usize);
  let mut scratch = ScratchSpace::new();
  filter_candidates(&seeds, 3, &precomp, &mut scratch);
  let mut expect = vec![
    [[1,1,0], [1,0,0], [0,0,0]],
    [[2,0,0], [1,0,0], [0,0,0]],
  ];
  expect.sort();
  assert_eq!(vec_to_points(&scratch.new_cubes, 3), expect);
}

fn next_polycubes_of_size(seeds: &[PolyCubeT], size: usize, precomp: &Precomputed, scratch:&mut ScratchSpace) {
  scratch.all_cubes.clear();
  scratch.candidate_filter.clear();
  for seed in seeds {
    propose_candidates(&seed, size, &precomp, scratch);
    filter_candidates(&scratch.candidates.clone(), size+1, &precomp, scratch);
    scratch.all_cubes.append(&mut scratch.new_cubes);
  }
}
#[test]
fn next_polycubes_of_size_test() {
  let precomp = precomputation_helper(point_to_idx([4,4,4]) as usize);
  let mut scratch = ScratchSpace::new();
  scratch.all_cubes.push(PolyCubeT::zeros());
  let expect = [1,2,8,29];
  for size in 1..5 {
    let all_cubes = scratch.all_cubes.clone();
    next_polycubes_of_size(&all_cubes, size, &precomp, &mut scratch);
    assert_eq!(scratch.all_cubes.len(), expect[size-1], "for size={}", size);
  }
}

fn main() {
  let precomp = precomputation();
  let mut scratch = ScratchSpace::new();
  scratch.all_cubes.push(PolyCubeT::zeros());

  for size in 1..MAX_SIZE {
    let now = time::Instant::now();
    let all_cubes = scratch.all_cubes.clone();
    next_polycubes_of_size(&all_cubes, size, &precomp, &mut scratch);
    println!("size={} cubes={}, secs={}", size+1, scratch.all_cubes.len(), now.elapsed().as_secs_f32());
  }
}
