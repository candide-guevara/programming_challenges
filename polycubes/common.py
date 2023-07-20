import contextlib
import logging
import numpy as np
import time
from constants import *

shift_vec = np.array([0, COORD_LEN, 2*COORD_LEN])
mask_vec = np.array([MASK_X, MASK_Y, MASK_Z])

def print_cube(cube):
  return ", ".join( str(idx_to_point(p).tolist()) for p in cube )

def idx_to_point(idx):
  return ((np.ones(DIMS, dtype=np.byte) * idx) >> shift_vec) & MAX_COORD

def point_to_idx(p):
  return (p << shift_vec).sum()

# offset must be positive, use 2-complement before calling
def shift_by(cube, size, offset):
  t = cube.copy()
  v = cube[:size]
  t[:size] = (v + offset) & MASK_X
  t[:size] |= (v + (offset & MASK_Y)) & MASK_Y
  t[:size] |= (v + (offset & MASK_Z)) & MASK_Z
  return t

# offset must be positive, use 2-complement before calling
def shift_by_vec_in_place(cubes, size, offsets):
  v = cubes[:,:size]
  #v_off = (offsets & mask_vec.reshape((-1,1))).reshape((3,-1,1))
  v_off = offsets.reshape((-1,1))
  cubes[:,:size] = ((v + v_off) & MASK_X
                   | (v + (v_off & MASK_Y)) & MASK_Y
                   | (v + (v_off & MASK_Z)) & MASK_Z)

def shift_to_origin(cube, size):
  v = cube[:size]
  of =  ((1 << COORD_LEN) - (v & MASK_X).min()) & MASK_X
  of |= ((1 << (2*COORD_LEN)) - (v & MASK_Y).min()) & MASK_Y
  of |= ((1 << (3*COORD_LEN)) - (v & MASK_Z).min()) & MASK_Z
  return shift_by(cube, size, of)

def sort_points(cube, size):
  t = np.unique(cube[:size])
  t2 = np.zeros(CUBE_ARR_LEN, dtype=np.ushort)
  t2[:t.shape[0]] = t
  return t2

def adj_cells_and_offsets(p):
  adj_cells = np.zeros(2*DIMS, dtype=np.ushort)
  adj_offset = np.zeros(2*DIMS, dtype=np.ushort)
  i = 0
  for d in range(DIMS):
    for o in (-1, 1):
      if p[d] == 0 and o == -1:
        adj_cells[i] = point_to_idx(p)
        adj_offset[i] = 1 << (d * COORD_LEN)
      elif p[d] == MAX_COORD and o == 1:
        adj_cells[i] = point_to_idx(p)
        adj_offset[i] = MAX_COORD << (d * COORD_LEN)
      else:
        p2 = p.copy()
        p2[d] += o
        adj_cells[i] = point_to_idx(p2)
        adj_offset[i] = 0
      i += 1
  return adj_cells, adj_offset

def create_cube(points):
  cube = np.zeros(CUBE_ARR_LEN, dtype=np.ushort)
  for i,p in enumerate(points):
    cube[i] = point_to_idx(p)
  return cube

def rotations_matrices():
  mi=np.array([[1,0,0], [0,1,0], [0,0,1]])
  mx=np.array([[1,0,0], [0,0,-1], [0,1,0]])
  my=np.array([[0,0,-1], [0,1,0], [1,0,0]])
  mz=np.array([[0,1,0], [-1,0,0], [0,0,1]])
  x_rots = [mi, mx, mx@mx, mx@mx@mx]
  y_rots = [mi, my, my@my, my@my@my]
  z_rots = [mi, mz, mz@mz, mz@mz@mz]
  matrices = {}
  for m0 in y_rots: matrices.update({ (m0@m1).tobytes():m0@m1 for m1 in x_rots })
  for m0 in z_rots: matrices.update({ (m0@m1).tobytes():m0@m1 for m1 in x_rots })
  return matrices.values()

# includes translation so that point referenced by `idx` always land in grid
def rotate_idx(m, idx):
  v = m @ idx_to_point(idx)
  v += m.min(axis=1) * (-MAX_COORD)
  return point_to_idx(v)

@contextlib.contextmanager
def print_time(scope):
  start = time.time()
  yield
  logging.info("%s: %d", scope, time.time()-start)

