import numpy as np
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
# The program you produced does not return correct results. For example for 4 cubes it returns a count of 150, the correct value is 8. can you correct the program?
#
# Result Attempt2: does not run
#
# Attempt3:
# The program you produced does not compile. I got the following error:
# ```
# File "copilot_gpt_4o.py", line 27, in generate_polycubes
#     return {frozenset(grid.nonzero())}
#             ^^^^^^^^^^^^^^^^^^^^^^^^^
# TypeError: unhashable type: 'numpy.ndarray'
# ```
# Can you fix the program so that it runs and produce teh correct results?
#
# Result Attempt3: results are wrong
#
# Attempt4:
# Ok now the program runs but does not return correct results. For example for 4 cubes it returns a count of 138, the correct value is 8. Looks like it is not de-duplicating the polycubes correctly. Can you correct the program?
#
# Result Attempt4: results are wrong
#
# Attempt5:
# It is getting better but the program still does not return correct results. For example for 4 cubes it returns a count of 66, the correct value is 8. Looks like it is not de-duplicating the polycubes based on all possible rotation and mirror symmetries. Can you correct the program?
#
# Result Attempt5: does not run
#
# Attempt6:
# The program you produced does not compile. I got the following error:
# ```
# File "copilot_gpt_4o.py", line 66, in normalize_position
#     positions -= positions.min(axis=0)
#                  ^^^^^^^^^^^^^
# AttributeError: 'frozenset' object has no attribute 'min'
# ```
# Can you fix the program so that it runs and produce the correct results?
#
# Result Attempt6: results are wrong

# Define the possible moves to adjacent cubes (6 possible directions)
moves = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

def is_valid(x, y, z, grid):
    """Check if the position is valid and unoccupied."""
    return 0 <= x < grid.shape[0] and 0 <= y < grid.shape[1] and 0 <= z < grid.shape[2] and grid[x, y, z] == 0

def rotate_3d(positions):
    """Generate all rotations of a set of 3D positions."""
    rotations = []
    positions = np.array(list(positions))
    for perm in product([1, -1], repeat=3):
        for axes in product(range(3), repeat=3):
            if len(set(axes)) == 3:
                rotated = np.dot(positions, np.diag(perm))[:, axes]
                rotations.append(frozenset(map(tuple, rotated)))
    return rotations

def normalize_polycube(positions):
    """Normalize the polycube to a canonical form considering all rotations and reflections."""
    positions = np.array(list(positions))
    min_positions = [normalize_position(pos) for pos in rotate_3d(positions)]
    canonical_position = min(min_positions, key=lambda pos: tuple(sorted(pos)))
    return canonical_position

def normalize_position(positions):
    """Normalize a set of positions by translating them to start from the origin."""
    positions = np.array(list(positions))  # Ensure positions is a numpy array
    positions -= positions.min(axis=0)
    return frozenset(map(tuple, positions))

def generate_polycubes(n, grid, x, y, z, remaining):
    """Generate all distinct polycubes of size n."""
    if remaining == 0:
        non_zero_positions = set(zip(*grid.nonzero()))
        return {normalize_polycube(non_zero_positions)}
    
    polycubes = set()
    for move in moves:
        nx, ny, nz = x + move[0], y + move[1], z + move[2]
        if is_valid(nx, ny, nz, grid):
            grid[nx, ny, nz] = 1
            polycubes |= generate_polycubes(n, grid, nx, ny, nz, remaining - 1)
            grid[nx, ny, nz] = 0
    return polycubes

def count_3d_polycubes(n):
    """Initialize the grid and start the counting."""
    grid_size = 2 * n  # Estimate the grid size to accommodate the polycube
    grid = np.zeros((grid_size, grid_size, grid_size), dtype=int)
    
    # Start from the center of the grid
    center = grid_size // 2
    grid[center, center, center] = 1
    
    polycubes = generate_polycubes(n, grid, center, center, center, n - 1)
    return len(polycubes)

def main():
	for size in range(3, MAX_SIZE):
		with print_time(""):
			polycubes = count_3d_polycubes(size)
			logging.info("size=%d, count=%d", size, polycubes)
			is_ok(size, polycubes)
	logging.warning("DONE")

if __name__ == '__main__':
	logging.basicConfig(level=logging.INFO,
											format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
	main()

