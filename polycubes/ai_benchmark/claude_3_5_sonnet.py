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
# Attempt2:
# This is new version is an improvement but the program still does not return correct results.
# For example for 4 cubes it returns a count of 37, the correct value is 8.
# Looks like it is not de-duplicating the polycubes based on all rotation, reflection and translation transformations.
# Can you fix the program?
#
# Result Attempt2: results are wrong
#
# Attempt3:
# It is getting better but the program still does not return correct results.
# For example for 4 cubes it returns a count of 27, the correct value is 8.
# Looks like it is not de-duplicating correctly.
# Can you fix the program?
#
# Result Attempt3: does not run
#
# Attempt4:
# This new program version does not run. I get the following exception:
# ```
# if not coords:
#        ^^^^^^
# ValueError: The truth value of an array with more than one element is ambiguous. Use a.any() or a.all()
# ```
# Can you fix the program so that it runs and produce the correct results?
#
# Result Attempt4: results are wrong
#

import numpy as np
from collections import deque

class PolycubeCounter:
    def __init__(self):
        # Six possible directions in 3D space (±x, ±y, ±z)
        self.directions = np.array([
            [1, 0, 0], [-1, 0, 0],
            [0, 1, 0], [0, -1, 0],
            [0, 0, 1], [0, 0, -1]
        ])
        # Pre-compute all possible transformations
        self.transformations = self._generate_transformations()

    def _generate_transformations(self):
        """Generate all possible 3D rotation and reflection matrices"""
        transformations = []
        # Generate all permutations of axes
        perms = [
            ((1,0,0), (0,1,0), (0,0,1)),
            ((1,0,0), (0,0,1), (0,-1,0)),
            ((1,0,0), (0,-1,0), (0,0,-1)),
            ((1,0,0), (0,0,-1), (0,1,0)),
            ((0,-1,0), (1,0,0), (0,0,1)),
            ((-1,0,0), (0,-1,0), (0,0,1)),
            ((0,1,0), (-1,0,0), (0,0,1)),
            ((0,0,-1), (0,1,0), (1,0,0)),
            ((-1,0,0), (0,1,0), (0,0,-1)),
            ((0,0,1), (0,1,0), (-1,0,0)),
            ((0,1,0), (0,0,1), (1,0,0)),
            ((0,0,1), (1,0,0), (0,1,0)),
            ((1,0,0), (0,0,-1), (0,-1,0)),
            ((0,-1,0), (0,0,1), (-1,0,0)),
            ((0,0,-1), (1,0,0), (0,-1,0)),
            ((-1,0,0), (0,0,-1), (0,-1,0)),
            ((0,1,0), (0,0,-1), (-1,0,0)),
            ((0,0,1), (-1,0,0), (0,-1,0)),
            ((0,-1,0), (-1,0,0), (0,0,-1)),
            ((0,0,-1), (0,-1,0), (-1,0,0)),
            ((0,1,0), (1,0,0), (0,0,-1)),
            ((-1,0,0), (0,0,1), (0,1,0)),
            ((0,0,-1), (-1,0,0), (0,1,0)),
            ((0,-1,0), (0,0,-1), (1,0,0))
        ]
        # Convert to matrices and add reflections
        for perm in perms:
            matrix = np.array(perm)
            transformations.append(matrix)
            # Add reflection across xy-plane
            transformations.append(matrix @ np.array([[1,0,0], [0,1,0], [0,0,-1]]))
            # Add reflection across xz-plane
            transformations.append(matrix @ np.array([[1,0,0], [0,-1,0], [0,0,1]]))
            # Add reflection across yz-plane
            transformations.append(matrix @ np.array([[-1,0,0], [0,1,0], [0,0,1]]))
        return transformations

    def normalize_coordinates(self, coords_list):
        """Normalize coordinates to start from (0,0,0)"""
        if len(coords_list) == 0:
            return tuple()
        # Convert input to numpy array if it isn't already
        coords = np.array(coords_list)
        min_coords = np.min(coords, axis=0)
        normalized = coords - min_coords
        # Convert to tuples and sort
        return tuple(sorted(map(tuple, normalized.tolist())))

    def get_canonical_form(self, coords_list):
        """Get canonical form considering all rotations and reflections"""
        coords = np.array(coords_list)
        canonical_forms = []
        for transform in self.transformations:
            # Apply transformation
            transformed = np.dot(coords, transform)
            # Normalize and add to list
            normalized = self.normalize_coordinates(transformed)
            canonical_forms.append(normalized)
        # Return the lexicographically smallest form
        return min(canonical_forms)

    def get_neighbors(self, coord):
        """Get all possible neighbors of a coordinate"""
        coord = np.array(coord)
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
                            # Important: normalize before getting canonical form
                            new_cubes = self.normalize_coordinates(new_cubes)
                            # Get canonical form for proper de-duplication
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

