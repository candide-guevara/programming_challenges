mod constants;
mod linear_alg;
mod precomputation;
mod utils;

use std::slice;
use constants::*;
use precomputation::*;
use utils::*;

struct ScratchSpace {
  offsets: Vec<AdjOffsetT>,
  candidates: Vec<PolyCubeT>,
}

impl ScratchSpace {
  fn fill_candidates_with(&mut self, cube: &PolyCubeT, size: usize) {
    self.candidates.clear();
    self.candidates.resize(2*DIMS*size, *cube);
  }
  fn select_offsets(&mut self, cube: &PolyCubeT, size: usize, precomp: &Precomputed) {
    self.offsets.clear();
    for c in &cube[0..size] { self.offsets.push(precomp.adj_offsets[*c as usize]); }
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
    };
  }
}

// From `cube` derive all possible polycubes of `size+1` which result from adding an adjacent cell. 
fn propose_candidates(cube: &PolyCubeT, size:usize, precomp: &Precomputed, scratch:&mut ScratchSpace) {
  scratch.fill_candidates_with(cube, size);
  scratch.select_offsets(cube, size, precomp);
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
  let cube = build_cube(&[[1,0,0], [0,0,0]]);
  let precomp = precomputation_helper(point_to_idx([3,3,3]) as usize);
  let mut scratch = ScratchSpace::new();
  propose_candidates(&cube, 2, &precomp, &mut scratch);
  let mut expect = vec![
    [[2,0,0], [1,0,0], [0,0,0]],
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

fn main() {
    println!("Hello, world!");
}
