import numpy as np
from collections import deque
import logging
from common import *

# Attempt1:
# Do you know what the counting 3D polycubes problem is?
# 
# Can you write a python program to count the number of possible 3D polycubes for any number of individual cubes? Use numpy to make the counting as fast as possible.
#
# Result Attempt1: results are wrong
# 
# Attempt2:
# I ran the program and it is not producing the right results. For example for `count_polycubes(4)` returns 136 instead of 8. Looks like it is not deduplicating correctly. Can you correct the program?
# 
# Result Attempt2: results are wrong
# 
# Attempt3:
# I replaced the `canonicalize` function with the new version you provided me but the results are the same, the number of polycubes is still not correct. Maybe try to rewrite the program from scratch to get a correct version?
# 
# Result Attempt3: results are wrong
# 

def count_polycubes(n):
    """
    Counts the number of distinct 3D polycubes of size n.

    Args:
        n: The number of cubes in the polycube.

    Returns:
        The number of distinct polycubes of size n.
    """

    if n <= 0:
        return 0

    # Start with a single cube at the origin
    initial_polycube = frozenset([(0, 0, 0)])
    queue = deque([initial_polycube])
    seen = {initial_polycube}
    count = 0

    for _ in range(n - 1):
        next_queue = deque()
        while queue:
            polycube = queue.popleft()
            for x, y, z in polycube:
                # Try adding a cube in all 6 directions
                for dx, dy, dz in [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]:
                    new_cube = (x + dx, y + dy, z + dz)
                    if new_cube not in polycube:
                        # Create a new polycube by adding the new cube
                        new_polycube = set(polycube)
                        new_polycube.add(new_cube)

                        # Canonicalize the new polycube
                        canonical_polycube = canonicalize(new_polycube)

                        # Check if this canonical form has been seen before
                        if canonical_polycube not in seen:
                            seen.add(canonical_polycube)
                            next_queue.append(frozenset(new_polycube))

        queue = next_queue

    # In the last iteration, the queue contains all the distinct polycubes of size n
    return len(queue)

def canonicalize(polycube):
    """
    Finds the canonical representation of a polycube.

    Translates the polycube to the origin, then generates all 24 rotations
    and reflections, and returns the lexicographically smallest representation.
    """
    
    # Convert to list for easier manipulation.
    polycube_list = list(polycube)

    # Translate to origin
    min_x = min(x for x, _, _ in polycube_list)
    min_y = min(y for _, y, _ in polycube_list)
    min_z = min(z for _, _, z in polycube_list)
    translated_polycube = [(x - min_x, y - min_y, z - min_z) for x, y, z in polycube_list]

    # Generate all orientations
    orientations = set()
    for i in range(4):
        for j in range(4):
            for k in range(4):
                rotated = rotate(translated_polycube, i, j, k)
                orientations.add(frozenset(rotated))
                orientations.add(frozenset(reflect(rotated, 'x')))
                orientations.add(frozenset(reflect(rotated, 'y')))
                orientations.add(frozenset(reflect(rotated, 'z')))

    # Find the lexicographically smallest
    return min(orientations)

def rotate(polycube, i, j, k):
    """Rotates the polycube i times around x-axis, j times around y-axis, k times around z-axis."""
    rotated = polycube
    for _ in range(i):
        rotated = [(x, z, -y) for x, y, z in rotated]  # Rotate around x-axis
    for _ in range(j):
        rotated = [(-z, y, x) for x, y, z in rotated]  # Rotate around y-axis
    for _ in range(k):
        rotated = [(-y, x, z) for x, y, z in rotated]  # Rotate around z-axis
    return rotated

def reflect(polycube, axis):
    """Reflects the polycube across the specified axis (x, y, or z)."""
    if axis == 'x':
        return [(-x, y, z) for x, y, z in polycube]
    elif axis == 'y':
        return [(x, -y, z) for x, y, z in polycube]
    elif axis == 'z':
        return [(x, y, -z) for x, y, z in polycube]


def main():
  for size in range(3, MAX_SIZE):
    with print_time(""):
      polycubes = count_polycubes(size)
      logging.info("size=%d, count=%d", size, polycubes)
      is_ok(size, polycubes)
  logging.warning("DONE")

if __name__ == '__main__':
  logging.basicConfig(level=logging.INFO,
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
  main()

