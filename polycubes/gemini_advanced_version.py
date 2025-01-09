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

def count_polycubes(n):
    """
    Counts the number of distinct 3D polycubes of size n.

    Args:
        n: The number of cubes in the polycube.

    Returns:
        The number of distinct polycubes of size n.
    """

    if n == 0:
        return 0
    if n == 1:
        return 1

    seen = set()
    queue = deque()
    initial_polycube = np.zeros((1, 3), dtype=int)
    queue.append(initial_polycube)
    seen.add(tuple(map(tuple, initial_polycube)))

    count = 0
    for _ in range(n - 1):
        next_level = set()
        while queue:
            polycube = queue.popleft()
            for i in range(polycube.shape[0]):
                for dx, dy, dz in [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]:
                    new_cube = polycube[i] + np.array([dx, dy, dz])
                    
                    # Check if the new cube already exists in the polycube
                    if not np.any(np.all(polycube == new_cube, axis=1)):
                        new_polycube = np.vstack([polycube, new_cube])
                        
                        # Canonicalize the polycube
                        canonical_polycube = canonicalize(new_polycube)

                        # Add to the next level if it's a new shape
                        canonical_tuple = tuple(map(tuple, canonical_polycube))
                        if canonical_tuple not in next_level:
                            next_level.add(canonical_tuple)

        # Convert tuples back to numpy arrays for the next iteration
        queue.extend(np.array(list(polycube)) for polycube in next_level)

        if _ == n - 2:
          count = len(next_level)
          
    return count

def canonicalize(polycube):
    """
    Finds the canonical representation of a polycube.

    This involves translating to the origin, and then considering all
    possible rotations and reflections to find the lexicographically
    smallest representation.
    """

    # Translate to origin
    min_coords = np.min(polycube, axis=0)
    polycube -= min_coords

    # Generate all rotations and reflections
    orientations = set()
    for i in range(4):
        for j in range(4):
            for k in range(4):
                rotated = np.copy(polycube)
                for _ in range(i):
                    rotated = rotate_x(rotated)
                for _ in range(j):
                    rotated = rotate_y(rotated)
                for _ in range(k):
                    rotated = rotate_z(rotated)

                # Add rotated and reflected orientations to the set (avoids duplicates)
                orientations.add(tuple(map(tuple, rotated)))
                orientations.add(tuple(map(tuple, reflect_x(rotated))))
                orientations.add(tuple(map(tuple, reflect_y(rotated))))
                orientations.add(tuple(map(tuple, reflect_z(rotated))))

    # Find lexicographically smallest and convert back to numpy array
    canonical = np.array(min(orientations))
    
    return canonical

def rotate_x(polycube):
    """Rotates a polycube 90 degrees around the x-axis."""
    rotation_matrix = np.array([[1, 0, 0],
                                [0, 0, -1],
                                [0, 1, 0]])
    return np.dot(polycube, rotation_matrix)

def rotate_y(polycube):
    """Rotates a polycube 90 degrees around the y-axis."""
    rotation_matrix = np.array([[0, 0, 1],
                                [0, 1, 0],
                                [-1, 0, 0]])
    return np.dot(polycube, rotation_matrix)

def rotate_z(polycube):
    """Rotates a polycube 90 degrees around the z-axis."""
    rotation_matrix = np.array([[0, -1, 0],
                                [1, 0, 0],
                                [0, 0, 1]])
    return np.dot(polycube, rotation_matrix)

def reflect_x(polycube):
  """Reflects a polycube through the plane x=0"""
  reflection_matrix = np.array([[-1, 0, 0],
                                [0, 1, 0],
                                [0, 0, 1]])
  return np.dot(polycube, reflection_matrix)

def reflect_y(polycube):
  """Reflects a polycube through the plane y=0"""
  reflection_matrix = np.array([[1, 0, 0],
                                [0, -1, 0],
                                [0, 0, 1]])
  return np.dot(polycube, reflection_matrix)

def reflect_z(polycube):
  """Reflects a polycube through the plane z=0"""
  reflection_matrix = np.array([[1, 0, 0],
                                [0, 1, 0],
                                [0, 0, -1]])
  return np.dot(polycube, reflection_matrix)


def main():
	for size in range(3, MAX_SIZE):
		with print_time(""):
			polycubes = count_polycubes(size)
			logging.info("size=%d, count=%d", size, polycubes)
	logging.warning("DONE")

if __name__ == '__main__':
	logging.basicConfig(level=logging.INFO,
											format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
	main()

