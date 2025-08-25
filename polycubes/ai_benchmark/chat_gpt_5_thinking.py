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

# Generate 24 rotation matrices for the cube (ignoring reflections)
def rotation_matrices():
    mats = []
    axes = [np.array([1,0,0]), np.array([0,1,0]), np.array([0,0,1])]
    # Rotations from coordinate permutations and sign flips with determinant=1
    for perm in product([0,1,2], repeat=3):
        if len(set(perm))<3: continue
        base = np.stack([axes[i] for i in perm], axis=1)
        for signs in product([-1,1], repeat=3):
            mat = base * signs
            if round(np.linalg.det(mat)) == 1:
                mats.append(mat)
    return mats

ROTATIONS = rotation_matrices()

# Apply rotation to all coordinates
def rotate(shape, mat):
    arr = np.array(list(shape))
    rotated = arr @ mat
    rotated = rotated.astype(int)
    # translate so min corner is at origin
    mins = rotated.min(axis=0)
    normalized = rotated - mins
    return frozenset(map(tuple, normalized))

# Get canonical form (smallest lexicographic representative across rotations)
def canonical(shape):
    forms = [rotate(shape, R) for R in ROTATIONS]
    return min(forms)

# Expand a shape by adding one adjacent cube
def expand(shape):
    result = []
    for (x,y,z) in shape:
        for dx,dy,dz in [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)]:
            new_cube = (x+dx, y+dy, z+dz)
            if new_cube not in shape:
                new_shape = set(shape)
                new_shape.add(new_cube)
                result.append(frozenset(new_shape))
    return result

# Generate all one-sided polycubes of size n
def polycubes(n):
    shapes = {frozenset([(0,0,0)])}
    for size in range(2, n+1):
        next_shapes = set()
        for shape in shapes:
            for new_shape in expand(shape):
                next_shapes.add(canonical(new_shape))
        shapes = next_shapes
    return shapes


def main():
  for size in range(1, MAX_SIZE):
    with print_time(""):
      count = len(polycubes(size))
      logging.info("size=%d, count=%d", size, count)
      is_ok(size, count)
  logging.warning("DONE")


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
    main()

