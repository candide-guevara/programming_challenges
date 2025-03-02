use super::constants::*;
use super::utils::*;

// Use i32 since coordinates can be negative.
pub type RotMatrixT = [[i32; DIMS]; DIMS];
pub type VectorT = [i32; DIMS];

pub const MI:RotMatrixT = [[1,0, 0], [0,1, 0], [0,0, 1]];
pub const MX:RotMatrixT = [[1,0, 0], [0,0, 1], [0,-1,0]];
pub const MY:RotMatrixT = [[0,0,-1], [0,1, 0], [1,0, 0]];
pub const MZ:RotMatrixT = [[0,1, 0], [-1,0,0], [0,0, 1]];

// Cannot use `std::ops::Mul` since both the trait and array are not defined in the crate :/
// I would need to wrap the array in a `struct` which makes initialization of values tedious.
pub trait Alg {
  fn m_mul(&self, rhs:&RotMatrixT) -> RotMatrixT;
  fn v_mul(&self, rhs:&PointT) -> VectorT;
  // We turn the cell index to its corresponding vector and rotate it.
  // Then we translate the rotated vector so that it land fully on the grid.
  // This only works for matrices in the group of 90deg rotations.
  fn i_mul_and_offset(&self, rhs:IdxT) -> IdxT;
  fn to_str(&self) -> String;
}

impl Alg for RotMatrixT {
  fn m_mul(&self, rhs:&RotMatrixT) -> RotMatrixT {
    let mut m = RotMatrixT::zeros();
    for i in 0..DIMS {
      for j in 0..DIMS {
        for k in 0..DIMS {
          m[i][j] += self[i][k] * rhs[k][j];
        }
      }
    }
    return m;
  }

  fn v_mul(&self, rhs:&PointT) -> VectorT {
    let mut v = VectorT::zeros();
    for (row,i) in self.iter().zip(v.iter_mut()) {
      for (col,j) in row.iter().zip(rhs.iter()) {
        *i += col * (*j as i32);
      }
    }
    return v;
  }

  fn i_mul_and_offset(&self, rhs:IdxT) -> IdxT {
    let v = self.v_mul(&idx_to_point(rhs));
    let mut p = PointT::zeros();
    for (i,row) in self.iter().enumerate() {
      let mut offset = 0;
      for j in row { offset |= if *j < 0 { MAX_COORD as i32 } else { 0 }; }
      p[i] = (offset + v[i]) as IdxT;
    }
    return point_to_idx(p);
  }

  fn to_str(&self) -> String {
    return self.iter().map(|r| format!("{:?}", r))
                      .collect::<Vec<String>>()
                      .join("\n");
  }
}

#[test]
fn mat_mat_multiply_test() {
  let m1 = [[1,0,0], [0,0,3], [1,1,0]];
  let m2 = [[1,0,0], [0,0,3], [1,1,0]];
  let expect = [[1,0,0], [3,3,0], [1,0,3]];
  assert_eq!(m1.m_mul(&m2), expect);
  assert_eq!(m1.m_mul(&MI), m1);
  assert_eq!(MI.m_mul(&MI), MI);
}

#[test]
fn mat_vector_multiply_test() {
  let p  = [1,2,3];
  let m  = [[1,0,0], [0,0,3], [1,1,0]];
  let expect = [1,9,3];
  assert_eq!(m.v_mul(&p), expect);
  assert_eq!(MI.v_mul(&p), [1,2,3]);
}

#[test]
fn i_mul_and_offset_test() {
  let idx = point_to_idx([1,2,3]);
  assert_eq!(MI.i_mul_and_offset(idx), idx);
  assert_eq!(idx_to_point(MX.i_mul_and_offset(idx)), [1,3,MAX_COORD-2]);
  assert_eq!(idx_to_point(MY.i_mul_and_offset(idx)), [MAX_COORD-3,2,1]);
  assert_eq!(idx_to_point(MZ.i_mul_and_offset(idx)), [2,MAX_COORD-1,3]);
}

