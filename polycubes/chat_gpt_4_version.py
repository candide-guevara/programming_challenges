import logging
import numpy as np
from utils.common import *

def rotate_np(positions, axis):
    """Rotate positions 90 degrees around the given axis using NumPy for efficiency."""
    if axis == 0:  # Rotate around x-axis
        return np.dot(positions, np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]]))
    elif axis == 1:  # Rotate around y-axis
        return np.dot(positions, np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]]))
    else:  # Rotate around z-axis
        return np.dot(positions, np.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]]))

def rotate(positions, axis):
    """Rotate positions 90 degrees around the given axis (0, 1, or 2 corresponding to x, y, z)."""
    if axis == 0:  # Rotate around x-axis
        return [(x, -z, y) for (x, y, z) in positions]
    elif axis == 1:  # Rotate around y-axis
        return [(z, y, -x) for (x, y, z) in positions]
    else:  # Rotate around z-axis
        return [(-y, x, z) for (x, y, z) in positions]

def all_rotations(positions):
    """Generate all distinct rotations of the given positions."""
    rotations = set()
    queue = [positions]
    while queue:
        current = queue.pop()
        for axis in range(3):
            rotated = rotate(current, axis)
            rotated_canonical = canonical_form(rotated)
            if rotated_canonical not in rotations:
                rotations.add(rotated_canonical)
                queue.append(rotated)
    return rotations

def all_rotations_np(positions):
    """Generate all distinct rotations of the given positions using NumPy."""
    rotations = set()
    positions_np = np.array(positions)
    queue = [positions_np]
    while queue:
        current = queue.pop(0)  # Pop from the start of the list
        for axis in range(3):
            rotated = rotate_np(current, axis)
            rotated_canonical = tuple(map(tuple, np.unique(rotated, axis=0)))
            if rotated_canonical not in rotations:
                rotations.add(rotated_canonical)
                queue.append(rotated)
    return rotations

def canonical_form_with_rotations_np(positions):
    """Find the canonical form considering all rotations, optimized with NumPy."""
    positions_np = np.array(positions)
    rotations = all_rotations_np(positions_np)
    # Convert each rotation to a sorted tuple of tuples and pick the minimal representation
    canonical_forms = [tuple(sorted(map(tuple, rotation))) for rotation in rotations]
    return min(canonical_forms)

def canonical_form_with_rotations(positions):
    """Find the canonical form considering all rotations."""
    rotations = all_rotations(positions)
    # Pick the minimal representation as the canonical form
    return min(rotations)

def canonical_form(positions):
    """Transform the set of positions into a canonical form to eliminate duplicates."""
    positions = [np.array(pos) for pos in positions]
    min_pos = np.min(positions, axis=0)
    positions = [tuple(pos - min_pos) for pos in positions]  # Normalize to origin
    # Sort to ensure unique representation
    positions.sort()
    return tuple(positions)

def canonical_form_opt(positions):
    """Generate a canonical form for a set of positions by normalizing their location."""
    min_x, min_y, min_z = np.min(positions, axis=0)
    # Normalize positions so the smallest x, y, z coordinates are moved to 0, 0, 0
    normalized_positions = positions - np.array([min_x, min_y, min_z])
    # Sort positions to have a consistent order
    sorted_positions = np.sort(normalized_positions, axis=0)
    return tuple(map(tuple, sorted_positions))

def generate_all_rotations(positions):
    """Generate all possible rotations of a set of positions."""
    rotations = {canonical_form_opt(positions)}
    for _ in range(3):  # Rotate around x-axis
        positions = rotate_np(positions, 0)
        rotations.add(canonical_form_opt(positions))
        for _ in range(3):  # Rotate around y-axis
            positions = rotate_np(positions, 1)
            rotations.add(canonical_form_opt(positions))
            for _ in range(3):  # Rotate around z-axis
                positions = rotate_np(positions, 2)
                rotations.add(canonical_form_opt(positions))
    return rotations

def add_cube(positions):
    """Add a new cube adjacent to the existing structure."""
    new_positions = []
    for pos in positions:
        x, y, z = pos
        for dx, dy, dz in [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]:
            new_pos = (x+dx, y+dy, z+dz)
            if new_pos not in positions:
                new_positions.append(positions + [new_pos])
    return new_positions

def normalize_positions(positions):
    """Normalize positions to ensure the polycube starts at (0, 0, 0) and is in a canonical form."""
    min_pos = positions.min(axis=0)
    normalized = positions - min_pos  # Translate to start at (0, 0, 0)
    return normalized

def generate_rotations(positions):
    """Efficiently generate all unique rotations of the given positions."""
    unique_rotations = set()
    for x in range(4):  # Four rotations around the x-axis
        for y in range(4):  # Four rotations around the y-axis
            for z in range(4):  # Four rotations around the z-axis
                rotated = positions
                for _ in range(x):
                    rotated = rotate_np(rotated, 0)
                for _ in range(y):
                    rotated = rotate_np(rotated, 1)
                for _ in range(z):
                    rotated = rotate_np(rotated, 2)
                normalized = normalize_positions(rotated)
                unique_rotations.add(tuple(map(tuple, np.unique(normalized, axis=0))))
    return unique_rotations

def get_all_rotations(shape):
    """Generate all unique rotations for a given shape."""
    rotations = set()
    shape = np.array(shape)

    # Define rotation matrices
    rot_x = np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]])
    rot_y = np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]])
    rot_z = np.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]])

    for cycle in range(2):  # Only need to cycle through twice due to symmetry
        for step_x in range(4):
            for step_y in range(4):
                for step_z in range(4):
                    rotated_shape = shape
                    for _ in range(step_x):
                        rotated_shape = rotated_shape @ rot_x
                    for _ in range(step_y):
                        rotated_shape = rotated_shape @ rot_y
                    for _ in range(step_z):
                        rotated_shape = rotated_shape @ rot_z

                    # Normalize the shape's position
                    min_x, min_y, min_z = np.min(rotated_shape, axis=0)
                    normalized_shape = rotated_shape - np.array([min_x, min_y, min_z])

                    rotations.add(tuple(map(tuple, normalized_shape)))

                    shape = shape @ rot_z  # Apply an additional rotation for the next cycle
                shape = shape @ rot_y
            shape = shape @ rot_x

    return rotations

def canonical(shape):
    """Convert the shape to its canonical form (sorted tuple)."""
    return tuple(sorted(shape))

def add_cube_opt(shape):
    """Try adding a cube to all faces of the current shape and return all unique new shapes."""
    new_shapes = []
    for x, y, z in shape:
        for dx, dy, dz in [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]:
            new_cube = (x + dx, y + dy, z + dz)
            if new_cube not in shape:
                new_shape = shape + [new_cube]
                new_shapes.append(canonical(new_shape))
    return new_shapes


def count_polycubes(n):
    if n == 1:
        return 1
    all_positions = [((0,0,0),)]
    for _ in range(n-1):
        new_all_positions = []
        seen = set()
        for positions in all_positions:
            for new_positions in add_cube(list(positions)):
                # Use canonical form with rotations to eliminate duplicates
                form = canonical_form_with_rotations(new_positions)
                if form not in seen:
                    seen.add(form)
                    new_all_positions.append(new_positions)
        all_positions = new_all_positions
    return len(all_positions)

def count_polycubes_optimized(n):
    """Count all unique polycubes of size n."""
    if n == 1:
        return 1  # Only one polycube for a single cube
    shapes = {((0,0,0),)}  # Starting shape
    for _ in range(1, n):
        new_shapes = set()
        for shape in shapes:
            for new_shape in add_cube_opt(list(shape)):
                rotations = get_all_rotations(list(new_shape))
                canonical_rotations = [canonical(list(rotation)) for rotation in rotations]
                new_shapes.update(canonical_rotations)
        shapes = new_shapes
    return len(shapes)


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

