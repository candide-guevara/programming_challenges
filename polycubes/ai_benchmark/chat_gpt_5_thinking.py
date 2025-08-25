import numpy as np
from collections import deque
from itertools import permutations, product
from typing import Set, Tuple, List
import logging
import time
from utils.common import *

# Attempt1:
# Do you know what the counting 3D polycubes problem is? If so, do you know the solutions for the first 2 to 6 cube counts?
#
# I am interested in one-sided counts where mirror images are considered different.
# Can you write a python program to count the number of possible 3D polycubes for any number of individual cubes? Use numpy to make the counting as fast as possible.
#
# Result Attempt1: results are wrong
#
# Attempt2:
# Looks like the program is overcounting the number of distinct one-sided polycubes. Here is the output it produced:
#
# One-sided polycubes of size 1: 1
# One-sided polycubes of size 2: 3
# One-sided polycubes of size 3: 15
# One-sided polycubes of size 4: 86
# One-sided polycubes of size 5: 534
# One-sided polycubes of size 6: 3481
#
# Rewrite the program to produce correct results.
#
# Result Attempt2: results are correct (almost as fast as hand written version)

# 6-neighborhood steps
NEIGHBORS = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]


def rotation_matrices():
  """Return the 24 proper (det=+1) lattice rotation matrices for a cube.

  Each matrix M is an integer 3x3 with orthonormal columns and det(M)=+1,
  mapping Z^3 -> Z^3 and representing a rotation (no reflections).
  """
  mats = []
  seen = set()
  I = np.eye(3, dtype=int)
  for perm in permutations(range(3)):
    P = I[:, list(perm)]        # permute columns (axes)
    for signs in product([-1, 1], repeat=3):
      S = np.diag(signs)
      M = P @ S            # sign flips on axes (columns)
      # keep only proper rotations (det = +1)
      if int(round(np.linalg.det(M))) == 1:
        key = tuple(M.flatten().tolist())
        if key not in seen:
          seen.add(key)
          mats.append(M)
  assert len(mats) == 24, f"Expected 24 rotations, got {len(mats)}"
  return mats


ROTATIONS = rotation_matrices()


def _normalize_coords(arr: np.ndarray) -> tuple:
  """Translate coords so the minimum xyz is at the origin and return a sorted tuple."""
  mins = arr.min(axis=0)
  shifted = arr - mins
  coords = sorted(map(tuple, shifted.tolist()))
  return tuple(coords)


def canonical(shape_set) -> tuple:
  """Canonical (rotation-invariant) representation as a tuple of sorted coords.

  Input: an iterable of (x,y,z) integer triples.
  Output: the lexicographically smallest normalized coordinate tuple across the 24 rotations.
  """
  arr = np.array(list(shape_set), dtype=int)
  reps = []
  for R in ROTATIONS:
    rot = arr @ R
    reps.append(_normalize_coords(rot))
  return min(reps)


def expand(shape_tuple):
  """Yield new shapes (as Python sets of coords) formed by adding one cube to the frontier."""
  s = set(shape_tuple)
  frontier = set()
  for (x, y, z) in s:
    for dx, dy, dz in NEIGHBORS:
      nb = (x + dx, y + dy, z + dz)
      if nb not in s:
        frontier.add(nb)
  for nb in frontier:
    new_s = set(s)
    new_s.add(nb)
    yield new_s


def one_sided_polycubes(n: int):
  """Return the set of canonical one-sided polycubes of size n."""
  if n < 1:
    return set()
  shapes = {((0, 0, 0),)} # canonical tuple for a single cube
  for size in range(2, n + 1):
    next_shapes = set()
    for shape in shapes:
      for new_shape in expand(shape):
        next_shapes.add(canonical(new_shape))
    shapes = next_shapes
  return shapes


def count_one_sided(n: int) -> int:
  return len(one_sided_polycubes(n))


def main():
  for size in range(1, MAX_SIZE):
    with print_time(""):
      count = count_one_sided(size)
      logging.info("size=%d, count=%d", size, count)
      is_ok(size, count)
  logging.warning("DONE")


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
    main()

