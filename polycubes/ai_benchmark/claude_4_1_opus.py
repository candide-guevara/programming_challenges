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
# Can you write a python program to count the number of possible 3D polycubes for any number of individual cubes? Use numpy to make the counting as fast as possible.
#
# Result Attempt1: results are wrong
#
# Attempt2:
# The program is not calculating the correct number of polycubes. Here is the output it produced:
#
# n=1:      1 (Expected:      1) ✓
# n=2:      1 (Expected:      1) ✓
# n=3:      2 (Expected:      2) ✓
# n=4:      7 (Expected:      8) ✗
# n=5:     23 (Expected:     29) ✗
# n=6:    112 (Expected:    166) ✗
#
# Can you fix the program?
#
# Result Attempt2: results are wrong
#
# Attempt3
# Unfortunately the new version is returning the same flawed results:
#
# n=1:      1 (Expected:      1) ✓
# n=2:      1 (Expected:      1) ✓
# n=3:      2 (Expected:      2) ✓
# n=4:      7 (Expected:      8) ✗
# n=5:     23 (Expected:     29) ✗
# n=6:    112 (Expected:    166) ✗
#
# Can you have another try at fixing the program?
#
# Result Attempt3: results are wrong (exact same flawed values)
#
# Attempt4:
# Same flawed results. Here is the output of debug_tetracubes:
#
#Found 7 unique tetracubes:
#
#straight: 1 polycube(s)
# straight #1:
#
#Polycube visualization:
#Layer z=0:
# ■
#
#Layer z=1:
#...
#
# Can you deduce what is wrong from the debug output and fix it?
#
# Result Attempt4: results are wrong (exact same flawed values)

class PolycubeCounter:
  def __init__(self):
    # Generate all 48 symmetries of the cube (24 rotations + 24 reflections)
    self.symmetries = self._generate_cube_symmetries()

  def _generate_cube_symmetries(self):
    """
    Generate all 48 symmetry transformations of a cube.
    This includes 24 rotational symmetries and 24 reflection symmetries.
    """
    symmetries = []

    # Generate the 24 rotational symmetries
    # For each of 6 face orientations, we have 4 rotations around that face

    # Identity and rotations around z-axis (0, 90, 180, 270 degrees)
    for k in range(4):
      angle = k * np.pi / 2
      c, s = int(np.cos(angle)), int(np.sin(angle))
      symmetries.append(np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]], dtype=int))

    # x face up (90 degree rotation around y-axis), then 4 rotations
    for k in range(4):
      angle = k * np.pi / 2
      c, s = int(np.cos(angle)), int(np.sin(angle))
      base = np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]], dtype=int)
      rot_z = np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]], dtype=int)
      symmetries.append(base @ rot_z)

    # -x face up (270 degree rotation around y-axis), then 4 rotations
    for k in range(4):
      angle = k * np.pi / 2
      c, s = int(np.cos(angle)), int(np.sin(angle))
      base = np.array([[0, 0, -1], [0, 1, 0], [1, 0, 0]], dtype=int)
      rot_z = np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]], dtype=int)
      symmetries.append(base @ rot_z)

    # y face up (90 degree rotation around x-axis), then 4 rotations
    for k in range(4):
      angle = k * np.pi / 2
      c, s = int(np.cos(angle)), int(np.sin(angle))
      base = np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]], dtype=int)
      rot_z = np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]], dtype=int)
      symmetries.append(base @ rot_z)

    # -y face up (270 degree rotation around x-axis), then 4 rotations
    for k in range(4):
      angle = k * np.pi / 2
      c, s = int(np.cos(angle)), int(np.sin(angle))
      base = np.array([[1, 0, 0], [0, 0, 1], [0, -1, 0]], dtype=int)
      rot_z = np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]], dtype=int)
      symmetries.append(base @ rot_z)

    # -z face up (180 degree rotation around x-axis), then 4 rotations
    for k in range(4):
      angle = k * np.pi / 2
      c, s = int(np.cos(angle)), int(np.sin(angle))
      base = np.array([[1, 0, 0], [0, -1, 0], [0, 0, -1]], dtype=int)
      rot_z = np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]], dtype=int)
      symmetries.append(base @ rot_z)

    # Also include reflections - for polycubes we need to check mirror images
    # Add a reflection (e.g., flip x-coordinate)
    num_rotations = len(symmetries)
    for i in range(num_rotations):
      reflected = symmetries[i] @ np.array([[-1, 0, 0], [0, 1, 0], [0, 0, 1]], dtype=int)
      symmetries.append(reflected)

    return symmetries

  def _canonical_form(self, cubes: List[Tuple[int, int, int]]) -> Tuple[Tuple[int, ...], ...]:
    """
    Find the canonical (lexicographically smallest) representation of a polycube.
    """
    if not cubes:
      return tuple()

    cubes_array = np.array(cubes, dtype=int)
    min_representation = None

    for symmetry in self.symmetries:
      # Apply transformation
      transformed = cubes_array @ symmetry.T

      # Translate so that minimum coordinates are at origin
      transformed = transformed - transformed.min(axis=0)

      # Sort lexicographically: first by x, then y, then z
      indices = np.lexsort((transformed[:, 2], transformed[:, 1], transformed[:, 0]))
      sorted_cubes = transformed[indices]

      # Convert to tuple representation
      representation = tuple(tuple(cube) for cube in sorted_cubes)

      # Keep the lexicographically smallest representation
      if min_representation is None or representation < min_representation:
        min_representation = representation

    return min_representation

  def _get_neighbors(self, cube: Tuple[int, int, int]) -> List[Tuple[int, int, int]]:
    """Get all 6 face-adjacent neighbors of a cube."""
    x, y, z = cube
    return [
      (x+1, y, z), (x-1, y, z),
      (x, y+1, z), (x, y-1, z),
      (x, y, z+1), (x, y, z-1)
    ]

  def generate_polycubes(self, n: int) -> Set[Tuple[Tuple[int, ...], ...]]:
    """Generate all unique polycubes with n unit cubes."""
    if n == 1:
      return {((0, 0, 0),)}

    # Build from smaller polycubes
    prev_polycubes = self.generate_polycubes(n - 1)
    new_polycubes = set()

    for poly in prev_polycubes:
      cubes = list(poly)
      cube_set = set(cubes)

      # Find all empty neighboring positions
      candidates = set()
      for cube in cubes:
        for neighbor in self._get_neighbors(cube):
          if neighbor not in cube_set:
            candidates.add(neighbor)

      # Try adding each candidate
      for candidate in candidates:
        extended_poly = cubes + [candidate]
        canonical = self._canonical_form(extended_poly)
        new_polycubes.add(canonical)

    return new_polycubes

  def count_polycubes(self, n: int) -> int:
    """Count the number of distinct polycubes with n unit cubes."""
    return len(self.generate_polycubes(n))

  def count_polycubes_up_to(self, max_n: int) -> List[int]:
    """Count polycubes for all sizes from 1 to max_n."""
    if max_n < 1:
      return []

    counts = []
    polycubes = {((0, 0, 0),)} # Start with single cube
    counts.append(1)

    for n in range(2, max_n + 1):
      with print_time(""):
        new_polycubes = set()

        for poly in polycubes:
          cubes = list(poly)
          cube_set = set(cubes)

          # Find all empty neighboring positions
          candidates = set()
          for cube in cubes:
            for neighbor in self._get_neighbors(cube):
              if neighbor not in cube_set:
                candidates.add(neighbor)

          # Try adding each candidate
          for candidate in candidates:
            extended_poly = cubes + [candidate]
            canonical = self._canonical_form(extended_poly)
            new_polycubes.add(canonical)

        polycubes = new_polycubes
        counts.append(len(polycubes))
      logging.info("size=%d, count=%d", n, counts[-1])
      is_ok(n, counts[-1])

    return counts

  def visualize_polycube(self, polycube):
    """ASCII visualization of a polycube."""
    if not polycube:
      return

    cubes = list(polycube)
    xs = [c[0] for c in cubes]
    ys = [c[1] for c in cubes]
    zs = [c[2] for c in cubes]

    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)
    min_z, max_z = min(zs), max(zs)

    cube_set = set(cubes)

    print("\nPolycube visualization:")
    for z in range(min_z, max_z + 1):
      print(f"Layer z={z}:")
      for y in range(max_y, min_y - 1, -1):
        row = ""
        for x in range(min_x, max_x + 1):
          if (x, y, z) in cube_set:
            row += "■ "
          else:
            row += "· "
        print(" " + row)
      print()

  def debug_tetracubes(self):
    """Debug function to show all tetracubes."""
    tetracubes = self.generate_polycubes(4)
    print(f"\nFound {len(tetracubes)} unique tetracubes:")

    for i, poly in enumerate(tetracubes, 1):
      print(f"\nTetracube #{i}:")
      self.visualize_polycube(poly)

      # Show some properties
      cubes = list(poly)
      xs = [c[0] for c in cubes]
      ys = [c[1] for c in cubes]
      zs = [c[2] for c in cubes]

      x_range = max(xs) - min(xs) + 1
      y_range = max(ys) - min(ys) + 1
      z_range = max(zs) - min(zs) + 1

      print(f" Dimensions: {x_range}×{y_range}×{z_range}")
      print(f" Cubes: {cubes}")


def main():
  """Example usage and verification against known values."""
  counter = PolycubeCounter()
  counter.count_polycubes_up_to(MAX_SIZE)
  # Debug tetracubes to see what we're missing
  if True: # Set to True to debug
    counter.debug_tetracubes()
  logging.warning("DONE")


if __name__ == "__main__":
  logging.basicConfig(level=logging.INFO,
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
  main()
