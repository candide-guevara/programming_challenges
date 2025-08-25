import numpy as np
from collections import deque
from functools import lru_cache
from itertools import permutations, product
import multiprocessing as mp
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
#
# Attempt3:
#
# Excellent this new version produces the correct results. However the computation for polycubes over 8 takes too much time. Optimize the program so it runs as fast as possible.
#
# Result Attempt3: results are correct and faster than hand written version (even without using multiprocessing)


NEIGHBORS = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]

# --- Rotation matrices (24 cube rotations) ---
def rotation_matrices():
  mats = []
  seen = set()
  I = np.eye(3, dtype=int)
  for perm in permutations(range(3)):
    P = I[:, list(perm)]
    for signs in product([-1, 1], repeat=3):
      S = np.diag(signs)
      M = P @ S
      if int(round(np.linalg.det(M))) == 1:
        key = tuple(M.flatten().tolist())
        if key not in seen:
          seen.add(key)
          mats.append(M)
  return mats

ROTATIONS = rotation_matrices()

# --- Helpers for canonicalization ---
@lru_cache(maxsize=None)
def canonical(shape_tuple):
  arr = np.array(shape_tuple, dtype=int)
  reps = []
  for R in ROTATIONS:
    rot = arr @ R
    mins = rot.min(axis=0)
    shifted = rot - mins
    reps.append(tuple(sorted(map(tuple, shifted.tolist()))))
  return min(reps)


def expand(shape_tuple):
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
    yield tuple(sorted(new_s))


# --- Parallelizable step ---
def _expand_and_canonical(shape):
  results = []
  for new_shape in expand(shape):
    results.append(canonical(new_shape))
  return results


def one_sided_polycubes(n: int, parallel: bool = True):
  if n < 1:
    return set()
  shapes = {((0, 0, 0),)}
  for size in range(2, n + 1):
    if parallel:
      with mp.Pool() as pool:
        all_new = pool.map(_expand_and_canonical, shapes)
      next_shapes = set()
      for group in all_new:
        next_shapes.update(group)
    else:
      next_shapes = set()
      for shape in shapes:
        for new_shape in expand(shape):
          next_shapes.add(canonical(new_shape))
    shapes = next_shapes
  return shapes


def count_one_sided(n: int, parallel: bool = True) -> int:
  return len(one_sided_polycubes(n, parallel))


def main():
  for size in range(1, MAX_SIZE):
    with print_time(""):
      count = count_one_sided(size, parallel=True)
      logging.info("size=%d, count=%d", size, count)
      is_ok(size, count)
  logging.warning("DONE")


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
    main()

