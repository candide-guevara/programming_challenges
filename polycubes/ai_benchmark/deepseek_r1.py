import logging
from itertools import product
from collections import deque
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
# Result Attempt2: results are wrong

# Precompute all 48 possible symmetry transformations (rotations + reflections)
def generate_symmetries():
    symmetries = []
    # Identity and rotations around axes
    for perm in [(0,1,2), (0,2,1), (1,0,2), (1,2,0), (2,0,1), (2,1,0)]:
        for sign in product([1,-1], repeat=3):
            if np.prod(sign) == 1:  # Proper rotations only
                mat = np.zeros((3,3), dtype=int)
                for i in range(3):
                    mat[i][perm[i]] = sign[i]
                if np.linalg.det(mat) == 1:
                    symmetries.append(mat)
    return symmetries

SYMMETRIES = generate_symmetries()

def normalize(polycube):
    """Translate polycube to origin and return canonical form"""
    polycube = np.array(list(polycube))
    min_coords = polycube.min(axis=0)
    translated = polycube - min_coords
    translated = [tuple(row) for row in translated]
    return frozenset(translated)

def canonical_form(polycube):
    """Find the canonical representation across all symmetries"""
    min_form = None
    for mat in SYMMETRIES:
        transformed = [tuple(mat.dot(np.array(point))) for point in polycube]
        normalized = normalize(transformed)
        sorted_normalized = tuple(sorted(normalized))
        if min_form is None or sorted_normalized < min_form:
            min_form = sorted_normalized
    return min_form

def count_polycubes(n):
    if n == 0:
        return 0
    # BFS approach to grow polycubes
    visited = set()
    queue = deque()
    # Start with the base cube at origin
    initial = frozenset([(0,0,0)])
    queue.append(initial)
    visited.add(canonical_form(initial))
    for _ in range(n-1):
        next_queue = deque()
        next_visited = set()
        while queue:
            pc = queue.popleft()
            # Generate all possible adjacent cubes
            for cube in pc:
                for dx, dy, dz in [(-1,0,0),(1,0,0),
                                 (0,-1,0),(0,1,0),
                                 (0,0,-1),(0,0,1)]:
                    neighbor = (cube[0]+dx, cube[1]+dy, cube[2]+dz)
                    if neighbor not in pc:
                        new_pc = frozenset(pc | {neighbor})
                        cf = canonical_form(new_pc)
                        if cf not in next_visited:
                            next_visited.add(cf)
                            next_queue.append(new_pc)
        queue = next_queue
        visited = next_visited
    return len(visited)

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

