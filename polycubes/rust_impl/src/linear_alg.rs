use super::constants::*;
use super::utils::*;

// Use i32 since coordinates can be negative.
pub type RotMatrixT = [[i32; DIMS]; DIMS];
pub type VectorT = [i32; DIMS];

// Cannot use `std::ops::Mul` since both the trait and array are not defined in the crate :/
// I would need to wrap the array in a `struct` which makes initialization of values tedious.
pub trait Alg {
  fn m_mul(&self, rhs:&RotMatrixT) -> RotMatrixT;
  fn v_mul(&self, rhs:&PointT) -> VectorT;
  // We turn the cell index to its corresponding vector and rotate it.
  // Then we translate the rotated vector so that it land fully on the grid.
  // This only works for matrices in the group of 90deg rotations.
  fn i_mul_and_shift(&self, rhs:IdxT) -> IdxT;
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

  fn i_mul_and_shift(&self, rhs:IdxT) -> IdxT {
    let v = self.v_mul(&idx_to_point(rhs));
    let mut p = PointT::zeros();
    for (i,row) in self.iter().enumerate() {
      let mut shift = 0;
      for j in row { shift = if *j < 0 { MAX_COORD } else { 0 }; }
      p[i] = shift + (v[i] as IdxT);
      debug_assert!(p[i] <= MAX_COORD);
    }
    return point_to_idx(p);
  }
}

#[test]
fn mat_mat_multiply_test() {
  let m1 = [[1,0,0], [0,0,3], [1,1,0]];
  let m2 = [[1,0,0], [0,0,3], [1,1,0]];
  let mi = [[1,0,0], [0,1,0], [0,0,1]];
  let expect = [[1,0,0], [3,3,0], [1,0,3]];
  assert_eq!(m1.m_mul(&m2), expect);
  assert_eq!(m1.m_mul(&mi), m1);
  assert_eq!(mi.m_mul(&mi), mi);
}

#[test]
fn mat_vector_multiply_test() {
  let p  = [1,2,3];
  let m  = [[1,0,0], [0,0,3], [1,1,0]];
  let mi = [[1,0,0], [0,1,0], [0,0,1]];
  let expect = [1,9,3];
  assert_eq!(m.v_mul(&p), expect);
  assert_eq!(mi.v_mul(&p), [1,2,3]);
}

#[test]
fn i_mul_and_shift_test() {
  let idx = point_to_idx([1,2,3]);
  let mi  = [[1,0,0], [0,1, 0], [0,0,1]];
  let mx  = [[1,0,0], [0,0,-1], [0,1,0]];
  assert_eq!(mi.i_mul_and_shift(idx), idx);
  assert_eq!(idx_to_point(mx.i_mul_and_shift(idx)), [1,MAX_COORD-3,2]);
}

