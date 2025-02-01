import logging
from itertools import product
from utils.common import *

# Attempt1: NOT using deepthink mode
# Do you know what the counting 3D polycubes problem is? If so, do you know the solutions for the first 2 to 6 cube counts?
#
# Can you write a python program to count the number of possible 3D polycubes for any number of individual cubes? Use numpy to make the counting as fast as possible.
#
# Result Attempt1: results are wrong
#
# Attempt2: ENABLED deepthink
# It is a very good start but the program still does not return correct results.
# For example for 4 cubes it returns a count of 150, the correct value is 8.
# Looks like it is not de-duplicating the polycubes based on all possible rotation and mirror symmetries. Can you correct the program?
#
# Result Attempt2: The server is busy. Please try again later. [3 times]

# Directions for neighboring cubes in 3D space
DIRECTIONS = [
    (1, 0, 0),
    (-1, 0, 0),
    (0, 1, 0),
    (0, -1, 0),
    (0, 0, 1),
    (0, 0, -1),
]

def is_valid_position(grid, position):
    """Check if a position is within the grid boundaries."""
    return all(0 <= position[i] < grid.shape[i] for i in range(3))

def add_cube(grid, position):
    """Add a cube to the grid at the given position."""
    grid[position] = 1

def remove_cube(grid, position):
    """Remove a cube from the grid at the given position."""
    grid[position] = 0

def count_polycubes(n, grid, position, visited, count):
    """Recursively count polycubes."""
    if n == 0:
        # If all cubes are placed, increment the count
        count[0] += 1
        return

    # Mark the current position as visited
    add_cube(grid, position)
    visited.add(position)

    # Try to place the next cube in all possible directions
    for direction in DIRECTIONS:
        new_position = tuple(np.add(position, direction))
        if is_valid_position(grid, new_position) and new_position not in visited:
            count_polycubes(n - 1, grid, new_position, visited, count)

    # Backtrack: remove the cube and unmark the position
    remove_cube(grid, position)
    visited.remove(position)

def polycube_count(n):
    """Count the number of polycubes for a given number of cubes."""
    # Initialize a 3D grid large enough to hold the polycube
    grid_size = 2 * n + 1  # Ensure the grid is large enough
    grid = np.zeros((grid_size, grid_size, grid_size), dtype=int)

    # Start with the first cube at the center of the grid
    start_position = (n, n, n)

    # Use a list to store the count (to pass by reference)
    count = [0]

    # Start the recursive counting process
    count_polycubes(n - 1, grid, start_position, set(), count)

    return count[0]

def main():
	for size in range(3, MAX_SIZE):
		with print_time(""):
			polycubes = polycube_count(size)
			logging.info("size=%d, count=%d", size, polycubes)
			is_ok(size, polycubes)
	logging.warning("DONE")

if __name__ == '__main__':
	logging.basicConfig(level=logging.INFO,
											format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
	main()

