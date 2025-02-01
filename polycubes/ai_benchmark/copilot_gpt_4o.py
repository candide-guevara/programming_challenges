import numpy as np
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
# Result Attempt2: does not compile

# Define the possible moves to adjacent cubes (6 possible directions)
moves = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

def is_valid(x, y, z, grid):
    """Check if the position is valid and unoccupied."""
    return 0 <= x < grid.shape[0] and 0 <= y < grid.shape[1] and 0 <= z < grid.shape[2] and grid[x, y, z] == 0

def generate_polycubes(n, grid, x, y, z, remaining):
    """Generate all distinct polycubes of size n."""
    if remaining == 0:
        return {frozenset(grid.nonzero())}
    
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

