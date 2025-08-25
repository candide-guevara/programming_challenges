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

class PolycubeCounter:
  def __init__(self):
    # Define all 24 rotation matrices (rotation group of the cube)
    self.rotations = self._generate_rotation_matrices()
  
  def _generate_rotation_matrices(self):
    """Generate all 24 rotation matrices for a cube."""
    rotations = []
    
    # Identity and rotations around x-axis
    for i in range(4):
      angle = i * np.pi / 2
      rot = np.array([
        [1, 0, 0],
        [0, np.cos(angle), -np.sin(angle)],
        [0, np.sin(angle), np.cos(angle)]
      ], dtype=np.int32)
      rotations.append(np.round(rot).astype(np.int32))
    
    # Rotations around y-axis (90 and 270 degrees)
    for i in [1, 3]:
      angle = i * np.pi / 2
      rot = np.array([
        [np.cos(angle), 0, np.sin(angle)],
        [0, 1, 0],
        [-np.sin(angle), 0, np.cos(angle)]
      ], dtype=np.int32)
      rotations.append(np.round(rot).astype(np.int32))
    
    # Rotations around z-axis combined with other orientations
    for face in range(6):
      if face < 4: # Already covered
        continue
      base_rot = rotations[face]
      for i in range(4):
        angle = i * np.pi / 2
        z_rot = np.array([
          [np.cos(angle), -np.sin(angle), 0],
          [np.sin(angle), np.cos(angle), 0],
          [0, 0, 1]
        ], dtype=np.int32)
        combined = np.round(z_rot @ base_rot).astype(np.int32)
        rotations.append(combined)
    
    # Complete the 24 rotations
    all_rotations = []
    bases = [
      np.eye(3, dtype=np.int32), # identity
      np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]], dtype=np.int32), # 90° around y
      np.array([[-1, 0, 0], [0, 1, 0], [0, 0, -1]], dtype=np.int32), # 180° around y
      np.array([[0, 0, -1], [0, 1, 0], [1, 0, 0]], dtype=np.int32), # 270° around y
      np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]], dtype=np.int32), # 90° around x
      np.array([[1, 0, 0], [0, 0, 1], [0, -1, 0]], dtype=np.int32), # 270° around x
    ]
    
    for base in bases:
      for i in range(4):
        angle = i * np.pi / 2
        z_rot = np.array([
          [np.cos(angle), -np.sin(angle), 0],
          [np.sin(angle), np.cos(angle), 0],
          [0, 0, 1]
        ], dtype=np.int32)
        all_rotations.append(np.round(base @ z_rot).astype(np.int32))
    
    return all_rotations[:24] # Take first 24 unique rotations
  
  def _normalize_polycube(self, cubes: np.ndarray) -> Tuple[Tuple[int, ...], ...]:
    """
    Convert a polycube to its canonical form by trying all rotations
    and reflections, then selecting the lexicographically smallest.
    """
    cubes = np.array(cubes)
    min_repr = None
    
    # Try all rotations
    for rot_matrix in self.rotations:
      rotated = cubes @ rot_matrix.T
      
      # Try both without and with reflection
      for reflect in [False, True]:
        if reflect:
          test_cubes = rotated.copy()
          test_cubes[:, 0] *= -1 # Reflect along x-axis
        else:
          test_cubes = rotated
        
        # Translate to origin (minimum at 0,0,0)
        test_cubes -= test_cubes.min(axis=0)
        
        # Sort cubes to get canonical ordering
        sorted_cubes = test_cubes[np.lexsort((test_cubes[:, 2], 
                           test_cubes[:, 1], 
                           test_cubes[:, 0]))]
        
        # Convert to tuple for comparison
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
    
    # Current set of polycubes
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


def main():
  """Example usage and verification against known values."""
  counter = PolycubeCounter()
  counter.count_polycubes_up_to(MAX_SIZE)
  logging.warning("DONE")


if __name__ == "__main__":
  logging.basicConfig(level=logging.INFO,
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
  main()
