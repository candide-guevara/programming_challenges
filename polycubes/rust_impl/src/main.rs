mod binary_heap;
mod bloom;
mod constants;
mod linear_alg;
mod precomputation;
mod scratch_space;
mod sort_polycubes;
mod utils;

use std::sync::{Arc, mpsc};
use std::thread;
use std::slice;
use std::time;

use constants::*;
use precomputation::*;
use scratch_space::*;
use sort_polycubes::*;
use utils::*;

// From `seed` derive all possible polycubes of `size+1` which result from adding an adjacent cell.
fn propose_candidates(seed: &PolyCubeT, size:usize, precomp: &Precomputed, scratch:&mut ProducerScratch) {
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
  let mut scratch = ProducerScratch::new();
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

fn propose_rot_candidates(candidate: &PolyCubeT, size: usize, precomp: &Precomputed, scratch:&mut RotatorScratch) {
  scratch.select_rotations(candidate, size, precomp);
  shift_to_origin_vec(&mut scratch.rot_candidates, size);
  let rot_len = sort_cube_cells_and_dedupe(&mut scratch.rot_candidates, size);
  scratch.rot_candidates.truncate(rot_len);
}
#[test]
fn propose_rot_candidates_test() {
  let precomp = precomputation_helper(point_to_idx([3,3,3]) as usize);
  let mut scratch = RotatorScratch::new();
  propose_rot_candidates(&build_cube(&[[1,1,0], [1,0,0], [0,0,0]]),
                         3, &precomp, &mut scratch);
  let mut expect = vec![
    [[0, 0, 1], [0, 1, 0], [0, 0, 0]],
    [[0, 0, 1], [1, 0, 0], [0, 0, 0]],
    [[0, 1, 0], [1, 0, 0], [0, 0, 0]],
    [[0, 1, 1], [0, 0, 1], [0, 0, 0]],
    [[0, 1, 1], [0, 0, 1], [0, 1, 0]],
    [[0, 1, 1], [0, 1, 0], [0, 0, 0]],
    [[1, 0, 1], [0, 0, 1], [0, 0, 0]],
    [[1, 0, 1], [0, 0, 1], [1, 0, 0]],
    [[1, 0, 1], [1, 0, 0], [0, 0, 0]],
    [[1, 1, 0], [0, 1, 0], [0, 0, 0]],
    [[1, 1, 0], [0, 1, 0], [1, 0, 0]],
    [[1, 1, 0], [1, 0, 0], [0, 0, 0]],
  ];
  expect.sort();
  assert_eq!(vec_to_points(&scratch.rot_candidates, 3), expect);
}

// Add each rotated polycube to the set of previously seen polycubes.
// If no rotated version was ever seen before, this is a new polycube.
fn filter_candidates(rot_candidates: &[PolyCubeT], scratch:&mut ConsumerScratch) {
  let mut dupes = 0;
  for candidate in rot_candidates {
    if !scratch.candidate_filter.insert(*candidate) { dupes += 1; }
  }
  if dupes == 0 { scratch.all_cubes.push(rot_candidates[0]); }
}
#[test]
fn filter_candidates_test() {
  let mut seeds = vec![
    build_cube(&[[1,1,0], [1,0,0], [0,0,0]]),
    build_cube(&[[0,0,1], [0,1,0], [0,0,0]]),
  ];
  let mut scratch = ConsumerScratch::new();
  filter_candidates(&seeds, &mut scratch);
  filter_candidates(&seeds, &mut scratch);
  seeds[0] = build_cube(&[[1,1,0], [0,1,0], [0,0,0]]);
  filter_candidates(&seeds, &mut scratch);
  let mut expect = vec![
    [[1,1,0], [1,0,0], [0,0,0]],
  ];
  expect.sort();
  assert_eq!(vec_to_points(&scratch.all_cubes, 3), expect);
}

fn next_polycubes_of_size(seeds: Vec<PolyCubeT>, size: usize,
                          precomp: Arc<Precomputed>) -> Vec<PolyCubeT> {
  let (tx, rx) = mpsc::sync_channel(1024);
  let (final_tx, final_rx) = mpsc::sync_channel(1);

  let producer_t = thread::spawn(move || {
    let mut scratch = ProducerScratch::new();
    let mut rscratch = RotatorScratch::new();
    for seed in seeds {
      propose_candidates(&seed, size, &precomp, &mut scratch);
      for candidate in &scratch.candidates {
        propose_rot_candidates(candidate, size+1, &precomp, &mut rscratch);
        tx.send(rscratch.rot_candidates.clone()).unwrap();
      }
    }
  });

  let consumer_t = thread::spawn(move || {
    let mut scratch = ConsumerScratch::new();
    for candidates in rx {
      filter_candidates(&candidates, &mut scratch);
    }
    final_tx.send(scratch.all_cubes).unwrap();
  });

  producer_t.join().unwrap();
  let r = final_rx.recv().unwrap();
  consumer_t.join().unwrap();
  return r;
}
#[test]
fn next_polycubes_of_size_test() {
  let precomp = precomputation_helper(point_to_idx([4,4,4]) as usize);
  let precomp = Arc::new(precomp);
  let mut seeds = vec![PolyCubeT::zeros()];

  let expect = [1,2,8,29];
  for size in 1..5 {
    seeds = next_polycubes_of_size(seeds.clone(), size, Arc::clone(&precomp));
    assert_eq!(seeds.len(), expect[size-1], "for size={}", size+1);
  }
}

fn main() {
  let precomp = Arc::new(precomputation());
  let mut seeds = vec![PolyCubeT::zeros()];

  for size in 1..MAX_SIZE {
    let now = time::Instant::now();
    seeds = next_polycubes_of_size(seeds.clone(), size, Arc::clone(&precomp));
    println!("size={} cubes={}, secs={}",
             size+1, seeds.len(), now.elapsed().as_secs_f32());
  }
}

