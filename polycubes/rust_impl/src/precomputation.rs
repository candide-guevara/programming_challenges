use super::constants::*;
use super::utils::*;

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

pub struct Precomputed {
  // For every possible cell in the grid its adjacent cells.
  // If the cell is on the boundary of the grid then the corresponding
  // offset indicates how to shift in order for both the cell and its adjacent
  // to fit in the grid.
  // For example:
  // * [1,1,1] has adjacent cells [0,1,1], [2,1,1] ... and nothing needs to be offset
  // * [0,0,0] has adjacent cells [0,0,0], [1,0,0] ... and offsets [1,0,0], [0,0,0] ...
  adj_cells: Vec<AdjCellsT>,
  adj_offsets: Vec<AdjOffsetT>,
}

fn precomputation() {
}

