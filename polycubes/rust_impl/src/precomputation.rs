use std::collections::HashSet;

use super::constants::*;
use super::linear_alg::*;
use super::utils::*;

// The indexes for all adjacent cells right/left per dimension.
pub type AdjCellsT = [IdxT; 2 * DIMS];
// An offset which you can add to a cell index to shift it.
// Offsets can be negative since we use modulo MAX_COORD arithmetic.
pub type AdjOffsetT = [IdxT; 2 * DIMS];
// For a given cell, stores all possible 90deg rotations.
// The rotation includes an offset so that the rotated cell lands on the grid.
pub type RotAndShiftT = [IdxT; POSSIBLE_ROTATIONS];

pub struct Precomputed {
  // For every possible cell in the grid store its adjacent cells.
  // If the cell is on the boundary of the grid then the corresponding
  // offset indicates how to shift in order for both the cell and its adjacent
  // to fit in the grid.
  // For example:
  // * [1,1,1] has adjacent cells [0,1,1], [2,1,1] ... and nothing needs to be offset
  // * [0,0,0] has adjacent cells [0,0,0], [1,0,0] ... and offsets [1,0,0], [0,0,0] ...
  pub adj_cells: Vec<AdjCellsT>,
  pub adj_offsets: Vec<AdjOffsetT>,

  // For every possible cell in the grid store its rotation transformations.
  pub rot_cells: Vec<RotAndShiftT>,
}

pub fn adj_cells_and_offsets_for_idx(idx: IdxT) -> (AdjCellsT, AdjOffsetT) {
  let p = idx_to_point(idx);
  let mut adj_cells = AdjCellsT::zeros();
  let mut adj_offsets = AdjOffsetT::zeros();
  let mut write_idx = 0;
  for d in 0..DIMS {
    for i in [-1i32,1i32] {
      if p[d] == 0 && i == -1 {
        adj_cells[write_idx] = idx;
        adj_offsets[write_idx] = 1 << SHIFTS[d];
      }
      else if p[d] == MAX_COORD && i == 1 {
        adj_cells[write_idx] = idx;
        adj_offsets[write_idx] = MAX_COORD << SHIFTS[d];
      }
      else {
        let mut p2 = p;
        p2[d] = (p2[d] as i32 + i) as IdxT;
        adj_cells[write_idx] = point_to_idx(p2);
        adj_offsets[write_idx] = 0;
      }
      write_idx += 1;
    }
  }
  return (adj_cells, adj_offsets);
}
#[test]
fn adj_cells_and_offsets_for_idx_test() {
  let p:PointT = [0,MAX_COORD,1];
  let idx = point_to_idx(p);
  let (cells, offsets) = adj_cells_and_offsets_for_idx(idx);
  let cell_points:Vec::<PointT> = cells.iter().map(|&i| idx_to_point(i)).collect();
  let offset_points:Vec::<PointT> = offsets.iter().map(|&i| idx_to_point(i)).collect();
  assert_eq!(cell_points[0], p);
  assert_eq!(offset_points[0], [1,0,0]);
  assert_eq!(cell_points[1], [1,MAX_COORD,1]);
  assert_eq!(offset_points[1], [0,0,0]);
  assert_eq!(cell_points[3], p);
  assert_eq!(offset_points[3], [0,MAX_COORD,0]);
}

fn calculate_group_90deg_rotations() -> Vec<RotMatrixT> {
  let mut matrices = HashSet::<RotMatrixT>::with_capacity(POSSIBLE_ROTATIONS);
  let x_rots = [MI, MX, MX.m_mul(&MX), MX.m_mul(&MX).m_mul(&MX)];
  let y_rots = [MI, MY, MY.m_mul(&MY), MY.m_mul(&MY).m_mul(&MY)];
  let z_rots = [MI, MZ, MZ.m_mul(&MZ), MZ.m_mul(&MZ).m_mul(&MZ)];
  for m0 in y_rots {
    for m1 in x_rots { matrices.insert(m0.m_mul(&m1)); }
  }
  for m0 in z_rots {
    for m1 in x_rots { matrices.insert(m0.m_mul(&m1)); }
  }
  return matrices.into_iter().collect();
}
#[test]
fn calculate_group_90deg_rotations_test() {
  let matrices = calculate_group_90deg_rotations();
  assert_eq!(matrices.len(), POSSIBLE_ROTATIONS);

  let mut mj = MI;
  for _ in 0..4 {
    assert!(matrices.iter().any(|m| *m == mj));
    mj = mj.m_mul(&MX);
  }
  assert!(matrices.iter().any(|m| *m == MX.m_mul(&MY)));
  assert!(matrices.iter().any(|m| *m == MZ.m_mul(&MY)));
}

pub fn precomputation() -> Precomputed {
  return precomputation_helper(IDX_MAX);
}
pub fn precomputation_helper(l:usize) -> Precomputed {
  let mut precomp = Precomputed{
    adj_cells:   Vec::<AdjCellsT>::with_capacity(l),
    adj_offsets: Vec::<AdjOffsetT>::with_capacity(l),
    rot_cells:   Vec::<RotAndShiftT>::with_capacity(l),
  };
  let matrices = calculate_group_90deg_rotations();
  for i in 0..l {
    let (cells, offsets) = adj_cells_and_offsets_for_idx(i as IdxT);
    let mut rots = RotAndShiftT::zeros();
    for (j, m) in matrices.iter().enumerate() {
      rots[j] = m.i_mul_and_offset(i as IdxT);
    }

    precomp.adj_cells.push(cells);
    precomp.adj_offsets.push(offsets);
    precomp.rot_cells.push(rots);
  }
  return precomp;
}
#[test]
fn precomputation_test() {
  let precomp = precomputation_helper(IDX_MAX / 8);
  assert!(precomp.adj_cells.len() > 0);
  assert!(precomp.adj_offsets.len() > 0);
  assert!(precomp.rot_cells.len() > 0);

  let cell = point_to_idx([1,0,0]) as usize;
  let m = MAX_COORD;
  let n = MAX_COORD-1;
  let mut expect = vec![
    [1,0,0], [1,m,0], [1,0,m], [1,m,m],
    [n,0,0], [n,m,0], [n,0,m], [n,m,m],
    [0,1,0], [m,1,0], [0,1,m], [m,1,m],
    [0,n,0], [m,n,0], [0,n,m], [m,n,m],
    [0,0,1], [m,0,1], [0,m,1], [m,m,1],
    [0,0,n], [m,0,n], [0,m,n], [m,m,n],
  ];
  let mut r_points = to_points(&precomp.rot_cells[cell], POSSIBLE_ROTATIONS);
  r_points.sort();
  expect.sort();
  assert_eq!(r_points, expect);
}

