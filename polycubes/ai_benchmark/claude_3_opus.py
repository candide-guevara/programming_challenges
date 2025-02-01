import numpy as np
from scipy.spatial.distance import cdist
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
# The program you produced does not return correct results. For example for 4 cubes it returns a count of 0, the correct value is 8. Can you correct the program?
#
# Result Attempt2: results are wrong

def is_connected(cubes):
    """Check if all cubes are connected."""
    dist = cdist(cubes, cubes, metric='chebyshev') <= 1
    visited = set()
    
    def dfs(cube):
        visited.add(tuple(cube))
        for neighbor in cubes[np.any(dist[tuple(cube)], axis=0)]:
            if tuple(neighbor) not in visited:
                dfs(neighbor)
    
    dfs(cubes[0])
    return len(visited) == len(cubes)

def normalize_polycube(cubes):
    """Translate polycube to origin and sort cubes."""
    cubes = cubes - np.min(cubes, axis=0)
    return tuple(map(tuple, sorted(cubes)))

def unique_polycubes(n):
    """Generate all unique polycubes of size n."""
    if n == 1:
        return {((0,0,0),)}

    polycubes = set()
    for polycube in unique_polycubes(n-1):
        polycube = np.array(polycube) 
        for cube in polycube:
            for direction in np.array([[0,0,1], [0,1,0], [1,0,0], [0,0,-1], [0,-1,0], [-1,0,0]]):
                new_cube = tuple(cube + direction)
                if new_cube not in polycube:
                    new_polycube = np.concatenate((polycube, [new_cube]))
                    if is_connected(new_polycube):
                        polycubes.add(normalize_polycube(new_polycube))
    
    return polycubes

def count_polycubes(n):
    """Count the number of unique 3D polycubes of size n."""
    return len(unique_polycubes(n))

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

