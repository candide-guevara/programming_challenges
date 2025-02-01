import numpy as np
from collections import deque
from itertools import product
import logging
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
        # Six possible directions in 3D space (±x, ±y, ±z)
        self.directions = np.array([
            [1, 0, 0], [-1, 0, 0],
            [0, 1, 0], [0, -1, 0],
            [0, 0, 1], [0, 0, -1]
        ])
        
    def normalize_coordinates(self, coords):
        """Normalize coordinates to start from (0,0,0)"""
        coords = np.array(coords)
        min_coords = np.min(coords, axis=0)
        return tuple(map(tuple, coords - min_coords))

    def get_canonical_form(self, coords):
        """Get canonical form considering all rotations"""
        coords = np.array(coords)
        
        # All possible 90-degree rotations in 3D
        rotations = [
            # Identity
            np.array([[1, 0, 0], [0, 1, 0], [0, 0, 1]]),
            # Rotations around x-axis
            np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]]),
            np.array([[1, 0, 0], [0, -1, 0], [0, 0, -1]]),
            np.array([[1, 0, 0], [0, 0, 1], [0, -1, 0]]),
            # Rotations around y-axis
            np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]]),
            np.array([[-1, 0, 0], [0, 1, 0], [0, 0, -1]]),
            np.array([[0, 0, -1], [0, 1, 0], [1, 0, 0]]),
            # Rotations around z-axis
            np.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]]),
            np.array([[-1, 0, 0], [0, -1, 0], [0, 0, 1]]),
            np.array([[0, 1, 0], [-1, 0, 0], [0, 0, 1]]),
            # Composite rotations
            np.array([[0, -1, 0], [0, 0, -1], [1, 0, 0]]),
            np.array([[0, 0, -1], [-1, 0, 0], [0, 1, 0]]),
            np.array([[-1, 0, 0], [0, 0, 1], [0, 1, 0]]),
            np.array([[0, 1, 0], [0, 0, 1], [1, 0, 0]]),
            np.array([[0, 0, 1], [1, 0, 0], [0, 1, 0]]),
            np.array([[1, 0, 0], [0, 0, -1], [0, -1, 0]]),
            np.array([[0, -1, 0], [0, 0, 1], [-1, 0, 0]]),
            np.array([[0, 0, -1], [1, 0, 0], [0, -1, 0]]),
            np.array([[-1, 0, 0], [0, 0, -1], [0, -1, 0]]),
            np.array([[0, 1, 0], [0, 0, -1], [-1, 0, 0]]),
            np.array([[0, 0, 1], [-1, 0, 0], [0, -1, 0]]),
            np.array([[0, -1, 0], [-1, 0, 0], [0, 0, -1]]),
            np.array([[0, 0, -1], [0, -1, 0], [-1, 0, 0]]),
            np.array([[0, 1, 0], [1, 0, 0], [0, 0, -1]])
        ]
        
        # Get all possible rotations of the polycube
        all_rotations = []
        for rot_matrix in rotations:
            rotated = np.dot(coords, rot_matrix)
            normalized = self.normalize_coordinates(rotated)
            all_rotations.append(normalized)
        
        # Return the lexicographically smallest rotation
        return min(all_rotations)

    def get_neighbors(self, coord):
        """Get all possible neighbors of a coordinate"""
        return [tuple(coord + d) for d in self.directions]

    def count_polycubes(self, n):
        """Count the number of unique polycubes with n cubes"""
        if n <= 0:
            return 0
        if n == 1:
            return 1

        seen = set()
        queue = deque()
        
        # Start with a single cube at origin
        initial = ((0, 0, 0),)
        queue.append(initial)
        seen.add(initial)
        
        # Generate polycubes incrementally from size 2 to n
        for size in range(2, n + 1):
            new_queue = deque()
            new_seen = set()
            
            while queue:
                current = queue.popleft()
                
                # Try adding a cube to each possible neighbor position
                for cube in current:
                    for neighbor in self.get_neighbors(cube):
                        if neighbor not in current:
                            # Create new polycube by adding neighbor
                            new_cubes = list(current)
                            new_cubes.append(neighbor)
                            new_cubes = tuple(sorted(new_cubes))
                            
                            # Get canonical form
                            canonical = self.get_canonical_form(new_cubes)
                            
                            if canonical not in new_seen:
                                new_seen.add(canonical)
                                new_queue.append(new_cubes)
            
            queue = new_queue
            seen = new_seen
            
            if size == n:
                return len(seen)

def main():
  counter = PolycubeCounter()
  for size in range(3, MAX_SIZE):
    with print_time(""):
      polycubes = counter.count_polycubes(size)
      logging.info("size=%d, count=%d", size, polycubes)
      is_ok(size, polycubes)
  logging.warning("DONE")

if __name__ == '__main__':
  logging.basicConfig(level=logging.INFO,
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
  main()

