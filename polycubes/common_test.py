import unittest
import numpy as np
from common import *

class Common(unittest.TestCase):
  
  def test_idx_to_from_point(self):
    for p in [(1,2,3), (0,0,0), (1,0,0), (0,1,0), (0,0,1)]:
      i = point_to_idx(p)
      p2 = idx_to_point(i)
      self.assertSequenceEqual(p, p2.tolist())

  def test_create_cube(self):
    points = [(1,2,3), (0,0,0), (3,2,1)]
    cube = create_cube(points)
    expect = np.zeros(CUBE_ARR_LEN, dtype=np.ushort)
    for i,p in enumerate(points): expect[i] = point_to_idx(p)
    self.assertSequenceEqual(cube.tolist(), expect.tolist())

  def test_shift_by_single_elt(self):
    cases = [((1,1,1), (1,0,0), (2,1,1),),
             ((1,1,1), (0,1,0), (1,2,1),), 
             ((1,1,1), (0,0,1), (1,1,2),), 
             ((1,1,1), (0,0,0), (1,1,1),), 
             ((1,1,1), (1,1,1), (2,2,2),), 
             ((1,1,1), (1,2,1), (2,3,2),), 
             ((3,3,3), (MAX_COORD,0,0), (2,3,3),),
             ((3,3,3), (0,MAX_COORD,0), (3,2,3),),
             ((3,3,3), (0,0,MAX_COORD), (3,3,2),),
             ((3,3,3), (MAX_COORD,MAX_COORD-1,MAX_COORD-2), (2,1,0),), ]
    for p,o,expect in cases:
      c = create_cube([p])
      c2 = create_cube([p])
      c = shift_by(c, 1, point_to_idx(o))
      c2 = shift_by(c2, 3, point_to_idx(o))
      self.assertSequenceEqual(idx_to_point(c[0]).tolist(), expect,
                               "size=1, p=%r, o=%r, expect=%r" % (p,o,expect))
      self.assertSequenceEqual(idx_to_point(c2[0]).tolist(), expect,
                               "size=3, p=%r, o=%r, expect=%r" % (p,o,expect))
      
  def test_shift_by_noop(self):
    c = create_cube([(1,2,3), (0,0,0), (3,2,1)])
    expect = create_cube([(1,2,3), (0,0,0), (3,2,1)])
    o = point_to_idx((0,0,0,))
    c = shift_by(c, 3, o)
    self.assertSequenceEqual(c.tolist(), expect.tolist())

  def test_shift_by(self):
    c = create_cube([(1,2,3), (0,0,0), (3,2,1)])
    expect = create_cube([(2,2,3), (1,0,0), (4,2,1)])
    o = point_to_idx((1,0,0,))
    c = shift_by(c, 3, o)
    self.assertSequenceEqual(c.tolist(), expect.tolist())

  def test_shift_by_and_sort(self):
    c0 = create_cube([(0,0,0), (0,1,0), (0,1,0)])
    expect = create_cube([(1,0,0), (0,1,0), (1,1,0)])
    o = point_to_idx((1,0,0,))
    for p in [[0,1], [1,0]]:
      c = c0.copy()
      c[:len(p)] = c0[p]
      c = shift_by(c, 2, o)
      c = sort_points(c, 3)
      self.assertSequenceEqual(c.tolist(), expect.tolist())

  def test_adj_cells_and_offsets_origin(self):
    p = idx_to_point(0)
    expect_cells = [0, point_to_idx((1,0,0,)),
                    0, point_to_idx((0,1,0,)),
                    0, point_to_idx((0,0,1,)),]
    expect_offset = [point_to_idx([1,0,0]), 0,
                     point_to_idx([0,1,0]), 0,
                     point_to_idx([0,0,1]), 0, ]
    adj_cells, adj_offset = adj_cells_and_offsets(p)
    self.assertSequenceEqual(adj_cells.reshape(-1).tolist(), expect_cells)
    self.assertSequenceEqual(adj_offset.tolist(), expect_offset)

  def test_adj_cells_and_offsets_middle(self):
    p = idx_to_point(point_to_idx([4,4,4]))
    expect_cells = [point_to_idx((3,4,4,)), point_to_idx((5,4,4,)),
                    point_to_idx((4,3,4,)), point_to_idx((4,5,4,)),
                    point_to_idx((4,4,3,)), point_to_idx((4,4,5,)),]
    expect_offset = [0, 0,
                     0, 0,
                     0, 0, ]
    adj_cells, adj_offset = adj_cells_and_offsets(p)
    self.assertSequenceEqual(adj_cells.reshape(-1).tolist(), expect_cells)
    self.assertSequenceEqual(adj_offset.tolist(), expect_offset)

  def test_adj_cells_and_offsets_upper(self):
    p = idx_to_point(point_to_idx([MAX_COORD, MAX_COORD, MAX_COORD,]))
    expect_cells = [point_to_idx((MAX_COORD-1,MAX_COORD,MAX_COORD,)), point_to_idx((MAX_COORD,MAX_COORD,MAX_COORD,)),
                    point_to_idx((MAX_COORD,MAX_COORD-1,MAX_COORD,)), point_to_idx((MAX_COORD,MAX_COORD,MAX_COORD,)),
                    point_to_idx((MAX_COORD,MAX_COORD,MAX_COORD-1,)), point_to_idx((MAX_COORD,MAX_COORD,MAX_COORD,)),]
    expect_offset = [0, point_to_idx((MAX_COORD,0,0,)),
                     0, point_to_idx((0,MAX_COORD,0,)),
                     0, point_to_idx((0,0,MAX_COORD,)), ]
    adj_cells, adj_offset = adj_cells_and_offsets(p)
    self.assertSequenceEqual(adj_cells.reshape(-1).tolist(), expect_cells)
    self.assertSequenceEqual(adj_offset.tolist(), expect_offset)

  def test_rotations_matrices(self):
    matrices = rotations_matrices()
    self.assertEqual(len(matrices), 24)
    vx = np.array([1,0,0])
    vy = np.array([0,1,0])
    vz = np.array([0,0,1])
    trans_x = set( (m @ vx).tobytes() for m in matrices )
    trans_y = set( (m @ vy).tobytes() for m in matrices )
    trans_z = set( (m @ vz).tobytes() for m in matrices )
    self.assertEqual(len(trans_x), 6)
    self.assertEqual(len(trans_y), 6)
    self.assertEqual(len(trans_z), 6)

  def test_rotate_idx_conserve_angles(self):
    matrices = rotations_matrices()
    rng = np.random.default_rng(12345)
    v0 = np.array([0,0,0])
    for i in range(32):
      v1 = rng.integers(MAX_COORD, size=DIMS)
      v2 = rng.integers(MAX_COORD, size=DIMS)
      expect_dot = (v1*v2).sum()
      for m in matrices:
        v0r = idx_to_point(rotate_idx(m, point_to_idx(v0))).astype(np.int32)
        v1r = idx_to_point(rotate_idx(m, point_to_idx(v1))).astype(np.int32)
        v2r = idx_to_point(rotate_idx(m, point_to_idx(v2))).astype(np.int32)
        dot = ((v1r-v0r)*(v2r-v0r)).sum()
        self.assertEqual(dot, expect_dot, "m=%r\nv0r=%r, v1r=%r, v2r=%r" % (m, v0r, v1r, v2r))

  def test_shift_to_origin(self):
    sizes = [1,2,3,1,3]
    cubes = [create_cube([(0,0,0),]),
             create_cube([(1,2,3), (0,0,0),]),
             create_cube([(1,2,3), (0,0,0), (2,5,4),]),
             create_cube([(1,2,4),]),
             create_cube([(1,2,3), (3,1,5), (2,5,2),]),
            ]
    expectations = [create_cube([(0,0,0),]),
                    create_cube([(1,2,3), (0,0,0),]),
                    create_cube([(1,2,3), (0,0,0), (2,5,4),]),
                    create_cube([(0,0,0),]),
                    create_cube([(0,1,1), (2,0,3), (1,4,0),]),
                   ]
    for c,size,expect in zip(cubes, sizes, expectations):
      c = shift_to_origin(c, size)
      self.assertSequenceEqual(c.tolist(), expect.tolist())

  def test_sort_points(self):
    sizes = [1,2,2,3,5]
    cubes = [create_cube([(0,0,0),]),
             create_cube([(0,2,0), (0,2,0),]),
             create_cube([(1,2,3), (0,0,0),]),
             create_cube([(1,2,3), (0,0,6), (0,5,6),]),
             create_cube([(1,2,3), (0,5,6), (0,0,6), (0,5,6), (1,2,3)]),
            ]
    expectations = [create_cube([(0,0,0),]),
                    create_cube([(0,2,0),]),
                    create_cube([(0,0,0), (1,2,3),]),
                    create_cube([(1,2,3), (0,0,6), (0,5,6),]),
                    create_cube([(1,2,3), (0,0,6), (0,5,6),]),
                   ]
    for c,size,expect in zip(cubes, sizes, expectations):
      c = sort_points(c, size)
      self.assertSequenceEqual(c.tolist(), expect.tolist(),
                               "c=%r expect=%r" % (print_cube(c), print_cube(expect)))
    
if __name__ == '__main__':
  unittest.main()

