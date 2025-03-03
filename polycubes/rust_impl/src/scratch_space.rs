use rustc_hash::FxHashSet;

use super::constants::*;
use super::precomputation::*;
use super::utils::*;

// Scratch space needed to compute candidates and dedupe them.
// Re-used to minimize allocations.
pub struct ProducerScratch {
  pub offsets: Vec<AdjOffsetT>,
  pub candidates: Vec<PolyCubeT>,
}
pub struct RotatorScratch {
  pub rot_candidates: Vec<PolyCubeT>,
}
pub struct ConsumerScratch {
  pub candidate_filter: FxHashSet<PolyCubeT>,
  pub all_cubes: Vec<PolyCubeT>,
}

impl ProducerScratch {
  pub fn fill_candidates_with(&mut self, seed: &PolyCubeT, size: usize) {
    self.candidates.clear();
    self.candidates.resize(2*DIMS*size, *seed);
  }
  pub fn select_offsets(&mut self, seed: &PolyCubeT, size: usize, precomp: &Precomputed) {
    self.offsets.clear();
    self.offsets.resize(size, AdjOffsetT::zeros());
    //for (c,o) in (&seed[0..size]).iter().zip(self.offsets.iter_mut()) {
    //  *o = precomp.adj_offsets[*c as usize];
    for j in 0..size {
      self.offsets[j] = precomp.adj_offsets[seed[j] as usize];
    }
  }
  pub fn append_adj_to_cubes(&mut self, size: usize, precomp: &Precomputed) {
    for i in 0..size {
      let adj_i = self.candidates[0][i] as usize;
      for j in 0..(2*DIMS) {
        self.candidates[2*DIMS*i+j][size] = precomp.adj_cells[adj_i][j];
      }
    }
  }
  pub fn new() -> ProducerScratch {
    return ProducerScratch {
      offsets: Vec::<AdjOffsetT>::with_capacity(MAX_SIZE),
      candidates: Vec::<PolyCubeT>::with_capacity(2*DIMS*MAX_SIZE),
    };
  }
}

impl RotatorScratch {
  pub fn select_rotations(&mut self, seed: &PolyCubeT, size: usize, precomp: &Precomputed) {
    self.rot_candidates.clear();
    self.rot_candidates.resize(POSSIBLE_ROTATIONS, PolyCubeT::zeros());
    for j in 0..size {
      for r in 0..POSSIBLE_ROTATIONS {
        self.rot_candidates[r][j] = precomp.rot_cells[seed[j] as usize][r];
      }
    }
  }
  pub fn new() -> RotatorScratch {
    return RotatorScratch {
      rot_candidates: Vec::<PolyCubeT>::with_capacity(POSSIBLE_ROTATIONS),
    };
  }
}

impl ConsumerScratch {
  pub fn new() -> ConsumerScratch {
    return ConsumerScratch {
      candidate_filter: FxHashSet::<PolyCubeT>::default(),
      all_cubes: Vec::<PolyCubeT>::with_capacity(2usize.pow(16)),
    };
  }
}

