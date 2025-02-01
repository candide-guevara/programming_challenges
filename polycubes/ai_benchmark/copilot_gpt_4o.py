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

# Define the possible moves to adjacent cubes (6 possible directions)
moves = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

def is_valid(x, y, z, grid):
    """Check if the position is valid and unoccupied."""
    return 0 <= x < grid.shape[0] and 0 <= y < grid.shape[1] and 0 <= z < grid.shape[2] and grid[x, y, z] == 0

def count_polycubes(n, grid, x, y, z, remaining, visited):
    """Count the number of distinct polycubes of size n."""
    if remaining == 0:
        return 1
    
    count = 0
    for move in moves:
        nx, ny, nz = x + move[0], y + move[1], z + move[2]
        if is_valid(nx, ny, nz, grid) and (nx, ny, nz) not in visited:
            grid[nx, ny, nz] = 1
            visited.add((nx, ny, nz))
            count += count_polycubes(n, grid, nx, ny, nz, remaining - 1, visited)
            grid[nx, ny, nz] = 0
            visited.remove((nx, ny, nz))
    return count

def count_3d_polycubes(n):
    """Initialize the grid and start the counting."""
    grid_size = 2 * n  # Estimate the grid size to accommodate the polycube
    grid = np.zeros((grid_size, grid_size, grid_size), dtype=int)
    
    # Start from the center of the grid
    center = grid_size // 2
    grid[center, center, center] = 1
    visited = {(center, center, center)}
    
    return count_polycubes(n, grid, center, center, center, n - 1, visited)

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

