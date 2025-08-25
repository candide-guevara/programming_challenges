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
# Result Attempt3: results are wrong

class PolycubeCounter:
  def __init__(self):
    # Generate all 48 symmetries (24 rotations + 24 reflections)
    self.symmetries = self._generate_all_symmetries()
  
  def _generate_all_symmetries(self):
    """
    Generate all 48 symmetry operations for a cube (including reflections).
    This includes all combinations of:
    - Permutations of axes (6 possibilities)
    - Sign changes (8 possibilities)
    But only those that preserve handedness (24) or reverse it (24).
    """
    symmetries = []
    
    # Generate all possible combinations
    for perm in permutations([0, 1, 2]):
      for signs in product([-1, 1], repeat=3):
        matrix = np.zeros((3, 3), dtype=int)
        for i, (j, sign) in enumerate(zip(perm, signs)):
          matrix[i, j] = sign
        
        # Check if this is a valid rotation or reflection
        det = np.linalg.det(matrix)
        if abs(det - 1) < 0.1 or abs(det + 1) < 0.1: # det = ±1
          symmetries.append(matrix)
    
    return symmetries
  
  def _canonical_form(self, cubes: List[Tuple[int, int, int]]) -> Tuple[Tuple[int, ...], ...]:
    """
    Find the canonical (lexicographically smallest) representation of a polycube
    under all symmetry operations.
    """
    cubes_array = np.array(cubes)
    min_representation = None
    
    for symmetry in self.symmetries:
      # Apply symmetry transformation
      transformed = (cubes_array @ symmetry.T)
      
      # Normalize: translate so min coordinates are at origin
      transformed -= transformed.min(axis=0)
      
      # Sort lexicographically
      order = np.lexsort((transformed[:, 2], transformed[:, 1], transformed[:, 0]))
      sorted_cubes = transformed[order]
      
      # Convert to tuple for comparison
      representation = tuple(map(tuple, sorted_cubes.tolist()))
      
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
    """
    Generate all unique polycubes with n unit cubes.
    Returns a set of canonical representations.
    """
    if n == 1:
      return {((0, 0, 0),)}
    
    # Start with all polycubes of size n-1
    smaller_polycubes = self.generate_polycubes(n - 1)
    new_polycubes = set()
    
    for poly in smaller_polycubes:
      poly_list = list(poly)
      poly_set = set(poly_list)
      
      # Find all adjacent empty positions
      candidates = set()
      for cube in poly_list:
        for neighbor in self._get_neighbors(cube):
          if neighbor not in poly_set:
            candidates.add(neighbor)
      
      # Try adding each candidate cube
      for candidate in candidates:
        new_poly = poly_list + [candidate]
        canonical = self._canonical_form(new_poly)
        new_polycubes.add(canonical)
    
    return new_polycubes
  
  def count_polycubes(self, n: int) -> int:
    """Count the number of distinct polycubes with n unit cubes."""
    return len(self.generate_polycubes(n))
  
  def count_polycubes_up_to(self, max_n: int) -> List[int]:
    """
    Count polycubes for all sizes from 1 to max_n.
    More efficient than calling count_polycubes repeatedly.
    """
    if max_n < 1:
      return []
    
    counts = []
    current_polycubes = {((0, 0, 0),)}
    counts.append(1)
    
    for n in range(2, max_n + 1):
      with print_time(""):
        new_polycubes = set()
        
        for poly in current_polycubes:
          poly_list = list(poly)
          poly_set = set(poly_list)
          
          # Find all adjacent empty positions
          candidates = set()
          for cube in poly_list:
            for neighbor in self._get_neighbors(cube):
              if neighbor not in poly_set:
                candidates.add(neighbor)
          
          # Try adding each candidate cube
          for candidate in candidates:
            new_poly = poly_list + [candidate]
            canonical = self._canonical_form(new_poly)
            new_polycubes.add(canonical)
        
        current_polycubes = new_polycubes
        count = len(current_polycubes)
        counts.append(count)
      logging.info("size=%d, count=%d", n, counts[-1])
      is_ok(n, counts[-1])
    
    return counts
  
  def visualize_polycube(self, polycube: Tuple[Tuple[int, ...], ...]):
    """
    ASCII visualization of a polycube shown layer by layer.
    """
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
    """
    Debug function to show all tetracubes and help identify missing ones.
    """
    tetracubes = self.generate_polycubes(4)
    print(f"\nFound {len(tetracubes)} unique tetracubes:")
    
    # Categorize by shape characteristics
    shapes = {
      'straight': [],  # I-shape: 4 in a line
      'L-shape': [],   # L-shape
      'T-shape': [],   # T-shape
      'Z-shape': [],   # Z or S shape
      'square': [],   # 2x2 flat square
      'corner': [],   # 3D corner shape
      'other': []
    }
    
    for poly in tetracubes:
      cubes = list(poly)
      xs = [c[0] for c in cubes]
      ys = [c[1] for c in cubes]
      zs = [c[2] for c in cubes]
      
      x_range = max(xs) - min(xs) + 1
      y_range = max(ys) - min(ys) + 1
      z_range = max(zs) - min(zs) + 1
      
      dims = sorted([x_range, y_range, z_range])
      
      if dims == [1, 1, 4]:
        shapes['straight'].append(poly)
      elif dims == [1, 2, 2]:
        shapes['square'].append(poly)
      elif dims == [1, 2, 3]:
        # Could be L or T
        if self._is_T_shape(cubes):
          shapes['T-shape'].append(poly)
        else:
          shapes['L-shape'].append(poly)
      elif dims == [2, 2, 2]:
        shapes['corner'].append(poly)
      else:
        shapes['other'].append(poly)
    
    for shape_type, polys in shapes.items():
      if polys:
        print(f"\n{shape_type}: {len(polys)} polycube(s)")
        for i, poly in enumerate(polys, 1):
          print(f" {shape_type} #{i}:")
          self.visualize_polycube(poly)
  
  def _is_T_shape(self, cubes: List[Tuple[int, int, int]]) -> bool:
    """Check if a tetracube forms a T shape."""
    # A T-shape has one cube that connects to 3 others
    connectivity = {}
    for cube in cubes:
      connectivity[cube] = 0
      for other in cubes:
        if cube != other:
          # Check if adjacent
          diff = sum(abs(a - b) for a, b in zip(cube, other))
          if diff == 1:
            connectivity[cube] += 1
    
    # T-shape has one cube with 3 connections
    return 3 in connectivity.values()


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
