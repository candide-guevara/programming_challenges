import numpy as np
from collections import deque
from itertools import product
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

class PolycubeCounter:
  def __init__(self):
    # Define all 24 rotation matrices (rotation group of the cube)
    self.rotations = self._generate_all_rotations()
  
  def _generate_all_rotations(self):
    """
    Generate all 24 rotation matrices for a cube.
    These represent all possible orientations of a cube in 3D space.
    """
    # Explicitly list all 24 rotation matrices
    # These are the symmetry group of the cube
    rotations = []
    
    # Identity
    rotations.append(np.array([[1, 0, 0], [0, 1, 0], [0, 0, 1]]))
    
    # Rotations around z-axis (90, 180, 270 degrees)
    rotations.append(np.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]]))
    rotations.append(np.array([[-1, 0, 0], [0, -1, 0], [0, 0, 1]]))
    rotations.append(np.array([[0, 1, 0], [-1, 0, 0], [0, 0, 1]]))
    
    # Rotations around x-axis (90, 180, 270 degrees)
    rotations.append(np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]]))
    rotations.append(np.array([[1, 0, 0], [0, -1, 0], [0, 0, -1]]))
    rotations.append(np.array([[1, 0, 0], [0, 0, 1], [0, -1, 0]]))
    
    # Rotations around y-axis (90, 180, 270 degrees)
    rotations.append(np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]]))
    rotations.append(np.array([[-1, 0, 0], [0, 1, 0], [0, 0, -1]]))
    rotations.append(np.array([[0, 0, -1], [0, 1, 0], [1, 0, 0]]))
    
    # Face rotations (cube on corner - 120 degree rotations)
    rotations.append(np.array([[0, 0, 1], [1, 0, 0], [0, 1, 0]]))
    rotations.append(np.array([[0, 1, 0], [0, 0, 1], [1, 0, 0]]))
    rotations.append(np.array([[0, -1, 0], [0, 0, 1], [-1, 0, 0]]))
    rotations.append(np.array([[0, 0, -1], [-1, 0, 0], [0, 1, 0]]))
    
    # Edge rotations (180 degree rotations around edge diagonals)
    rotations.append(np.array([[0, 1, 0], [1, 0, 0], [0, 0, -1]]))
    rotations.append(np.array([[0, -1, 0], [-1, 0, 0], [0, 0, -1]]))
    rotations.append(np.array([[-1, 0, 0], [0, 0, 1], [0, 1, 0]]))
    rotations.append(np.array([[-1, 0, 0], [0, 0, -1], [0, -1, 0]]))
    rotations.append(np.array([[0, 0, 1], [0, -1, 0], [1, 0, 0]]))
    rotations.append(np.array([[0, 0, -1], [0, -1, 0], [-1, 0, 0]]))
    
    # More combined rotations to complete the 24
    rotations.append(np.array([[0, 0, -1], [1, 0, 0], [0, -1, 0]]))
    rotations.append(np.array([[0, -1, 0], [0, 0, -1], [1, 0, 0]]))
    rotations.append(np.array([[0, 1, 0], [0, 0, -1], [-1, 0, 0]]))
    rotations.append(np.array([[0, 0, 1], [-1, 0, 0], [0, -1, 0]]))
    
    return rotations
  
  def _normalize_polycube(self, cubes: List[Tuple[int, int, int]]) -> Tuple[Tuple[int, ...], ...]:
    """
    Convert a polycube to its canonical form by trying all rotations
    and reflections, then selecting the lexicographically smallest.
    """
    cubes_array = np.array(cubes)
    min_repr = None
    
    # Try all 24 rotations
    for rot_matrix in self.rotations:
      rotated = cubes_array @ rot_matrix.T
      
      # Also try with reflection (48 total orientations)
      for reflect_multiplier in [1, -1]:
        test_cubes = rotated.copy()
        test_cubes[:, 0] *= reflect_multiplier
        
        # Translate so minimum coordinates are at origin
        test_cubes -= test_cubes.min(axis=0)
        
        # Sort cubes lexicographically
        sorted_indices = np.lexsort((test_cubes[:, 2], 
                      test_cubes[:, 1], 
                      test_cubes[:, 0]))
        sorted_cubes = test_cubes[sorted_indices]
        
        # Convert to tuple representation
        repr_tuple = tuple(map(tuple, sorted_cubes))
        
        if min_repr is None or repr_tuple < min_repr:
          min_repr = repr_tuple
    
    return min_repr
  
  def _get_neighbors(self, cube: Tuple[int, int, int]) -> List[Tuple[int, int, int]]:
    """Get all 6 face-adjacent neighbors of a cube."""
    x, y, z = cube
    return [
      (x+1, y, z), (x-1, y, z),
      (x, y+1, z), (x, y-1, z),
      (x, y, z+1), (x, y, z-1)
    ]
  
  def count_polycubes(self, n: int) -> int:
    """
    Count the number of distinct polycubes with n unit cubes.
    Uses BFS to generate all possible polycubes and canonical forms to eliminate duplicates.
    """
    if n == 1:
      return 1
    
    # Start with a single cube at origin
    initial = [(0, 0, 0)]
    polycubes = {self._normalize_polycube(initial)}
    
    # Build up polycubes iteratively
    for size in range(2, n + 1):
      new_polycubes = set()
      
      for poly in polycubes:
        poly_list = list(poly)
        poly_set = set(poly_list)
        
        # Find all possible positions for the next cube
        candidates = set()
        for cube in poly_list:
          for neighbor in self._get_neighbors(cube):
            if neighbor not in poly_set:
              candidates.add(neighbor)
        
        # Try adding each candidate
        for candidate in candidates:
          new_poly = poly_list + [candidate]
          canonical = self._normalize_polycube(new_poly)
          new_polycubes.add(canonical)
      
      polycubes = new_polycubes
      print(f"n={size}: {len(polycubes)} polycubes")
    
    return len(polycubes)
  
  def count_polycubes_up_to(self, max_n: int) -> List[int]:
    """
    Count polycubes for all sizes from 1 to max_n.
    More efficient than calling count_polycubes repeatedly.
    """
    if max_n < 1:
      return []
    
    counts = [1] # n=1
    
    if max_n == 1:
      return counts
    
    # Current set of polycubes (start with single cube)
    current = {((0, 0, 0),)}
    
    for n in range(2, max_n + 1):
      with print_time(""):
        next_polycubes = set()
        
        for poly in current:
          poly_list = list(poly)
          poly_set = set(poly_list)
          
          # Find all possible positions for the next cube
          candidates = set()
          for cube in poly_list:
            for neighbor in self._get_neighbors(cube):
              if neighbor not in poly_set:
                candidates.add(neighbor)
          
          # Try adding each candidate
          for candidate in candidates:
            new_poly = poly_list + [candidate]
            canonical = self._normalize_polycube(new_poly)
            next_polycubes.add(canonical)
        
        current = next_polycubes
        counts.append(len(current))
      logging.info("size=%d, count=%d", n, counts[-1])
      is_ok(n, counts[-1])
    
    return counts
  
  def visualize_polycube(self, polycube: List[Tuple[int, int, int]]):
    """
    Simple ASCII visualization of a polycube (for debugging).
    Shows layer-by-layer view.
    """
    if not polycube:
      return
    
    cubes = np.array(polycube)
    min_coords = cubes.min(axis=0)
    max_coords = cubes.max(axis=0)
    
    print("\nPolycube visualization (layer by layer):")
    for z in range(min_coords[2], max_coords[2] + 1):
      print(f"Layer z={z}:")
      layer = cubes[cubes[:, 2] == z]
      if len(layer) == 0:
        print(" (empty)")
        continue
      
      for y in range(max_coords[1], min_coords[1] - 1, -1):
        row = ""
        for x in range(min_coords[0], max_coords[0] + 1):
          if any((cube[0] == x and cube[1] == y) for cube in layer):
            row += "■ "
          else:
            row += "· "
        print(" " + row)
      print()


def main():
  """Example usage and verification against known values."""
  counter = PolycubeCounter()
  counter.count_polycubes_up_to(MAX_SIZE)
  logging.warning("DONE")


if __name__ == "__main__":
  logging.basicConfig(level=logging.INFO,
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
  main()
