# This took about 1h of generate-run-fix iterations with chatgpt.
# It generates polycube counts for size 4 in about 15s.
# This is several order of magnitude slower than my version.
# The results are wrong since they do not deduplicate all rotations/translations.
# However the results are not that far off (at least for size 3 and 4).
import numpy as np
import time

MAX_SIZE = 12
COORD_LEN = 4
DIMS = 3

# Generate all possible directions for adjacent cells
DIRECTIONS = np.array([[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1]])

def generate_adjacent_cells(p):
    # Generate all adjacent cells to a given point
    return p + DIRECTIONS

def create_cube(points):
    # Create a unique identifier for the cube based on the points' coordinates
    return tuple(map(tuple, points))

def add_unique_cube(cubes, new_cube):
    # Add a unique cube to the list of cubes (ignores duplicates)
    cube_id = create_cube(new_cube)
    cubes[cube_id] = new_cube

def find_new_cubes(cube, size):
    # Find all possible adjacent cubes to the given cube
    new_cubes = {}
    for p in cube:
        adjacent_cells = generate_adjacent_cells(p)
        for adj in adjacent_cells:
            if np.all(adj >= 0) and np.all(adj < 2**COORD_LEN):
                new_cube = np.vstack((cube, adj))
                if len(new_cube) == size:
                    add_unique_cube(new_cubes, new_cube)
                elif len(new_cube) < size:
                    sub_cubes = find_new_cubes(new_cube, size)
                    new_cubes.update(sub_cubes)
    return new_cubes

def count_polycubes(max_size):
    # Count unique polycubes up to the specified max_size
    polycube_counts = [1]  # Only the single 1x1x1 cube is counted at size 1
    for size in range(2, max_size + 1):
        unique_cubes = {}
        print(int(time.time()), polycube_counts)
        for z in range(2**COORD_LEN):
            for y in range(2**COORD_LEN):
                for x in range(2**COORD_LEN):
                    start_cube = np.array([[x, y, z]], dtype=np.uint16)
                    new_cubes = find_new_cubes(start_cube, size)

                    # Deduplicate the new cubes based on canonical representation
                    for cube in new_cubes.values():
                        sorted_cube = np.unique(cube, axis=0)
                        origin_cube = sorted_cube - sorted_cube.min(axis=0)
                        add_unique_cube(unique_cubes, origin_cube)

        polycube_counts.append(len(unique_cubes))
    return polycube_counts

if __name__ == '__main__':
    result = count_polycubes(MAX_SIZE)
    for size, count in enumerate(result, start=1):
        print(f"Size {size}: {count} unique polycubes.")

