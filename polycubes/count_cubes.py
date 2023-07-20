import logging
import numpy as np
from common import *

ADJ_CELLS = np.zeros((IDX_MAX, 2*DIMS), dtype=np.ushort)
ADJ_OFFSET = np.zeros((IDX_MAX, 2*DIMS), dtype=np.ushort)
ROT_MAP = np.zeros((IDX_MAX, POSSIBLE_ROTATIONS), dtype=np.ushort)

def calculate_keys_for_all_syms(cube, size):
  return set( sort_points(shift_to_origin(c, size),size).tobytes() for c in ROT_MAP[cube[:size]].T )

# We assume that the lower bound is always (0,0,0,)
def propose_candidates(cube, size):
  offsets = ADJ_OFFSET[cube[:size]].reshape(-1)
  candidates = np.tile(cube, reps=(offsets.shape[0],1))
  candidates[:,size] = ADJ_CELLS[cube[:size]].reshape(-1)
  shift_by_vec_in_place(candidates, size, offsets)
  filter_same = {}

  for candidate in candidates:
    candidate[:] = sort_points(candidate, size+1)
    if candidate[size] == 0: continue
    filter_same[candidate.tobytes()] = candidate
  #for c in filter_same.values(): print("...", c)
  return filter_same.values()

def filter_candidates(dupes, cube_candidates, size):
  new_cubes = []
  for c in cube_candidates:
    keys = calculate_keys_for_all_syms(c,size)
    if all( k not in dupes for k in keys ):
      new_cubes.append(c)
    dupes.update(keys)
  return new_cubes

def increase_by_one(polycubes, size):
  dupes = set()
  new_cubes = []
  for cube in polycubes:
    cube_candidates = propose_candidates(cube, size)
    new_cubes.extend(filter_candidates(dupes, cube_candidates, size+1))
  return new_cubes

def precalculations():
  for i in range(IDX_MAX):
     ADJ_CELLS[i], ADJ_OFFSET[i] = adj_cells_and_offsets(idx_to_point(i))
     logging.debug("*** ", idx_to_point(i).tolist())
     logging.debug("*** cell", i, ADJ_CELLS[i].tolist())
     logging.debug("*** off", i, ADJ_OFFSET[i].tolist())
     for j,r_mat in enumerate(rotations_matrices()):
       ROT_MAP[i][j] = rotate_idx(r_mat, i)


def main():
  precalculations()
  polycubes = [ create_cube([(0,0,0), (0,1,0),]) ]
  for size in range(2, MAX_SIZE):
    with print_time("size=%d" % size): polycubes = increase_by_one(polycubes, size)
    logging.info("size=%d, count=%d", size+1, len(polycubes))
    #for p in polycubes: print([ idx_to_point(pi).tolist() for pi in p[:size+1] ])
  logging.warning("DONE")

if __name__ == '__main__':
  logging.basicConfig(level=logging.INFO,
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
  main()

