import bisect
import multiprocessing
import logging
import os

import util
import io_util
import replay_pb2
import timeserie_pb2

REPLAY_EXT = 'aoe2record'

def load_repo_from_file(conf):
  repo = replay_pb2.ReplayRepo()
  if not conf.rebuild:
    io_util.parse_from_gz_file(conf.replay_repo, repo)
  return repo

def find_replay_files(conf, replay_repo):
  existing = set( r.filepath for r in replay_repo.replays )
  for dirpath, dirnames, filenames in os.walk(conf.aoe2_replay_dir):
    for replay_name in filter(lambda s: s.lower().endswith(REPLAY_EXT), filenames):
      replay_path = os.path.join(dirpath, replay_name)
      if replay_path in existing: continue
      stat_res = os.stat(replay_path)
      replay = replay_repo.replays.add()
      # this only works since on my filesystems I disable access time writing
      replay.start_secs = int(stat_res.st_atime)
      replay.end_secs = int(stat_res.st_mtime)
      replay.filepath = replay_path

def is_partially_inside(replay, start, end):
  return replay.start_secs <= end and replay.end_secs >= start

def is_inside(replay, start, end):
  return replay.start_secs >= start and replay.end_secs <= end

def extract_timeserie_time_bounds(entry):
  # HasField() got removed in proto3
  if not entry.filepath or not entry.start_secs:
    raise Exception('corrupted timeserie entry: %r' % entry)
  if not entry.end_secs:
    logging.warning("%r has no end time, trying to calculate it", entry)
    ref_secs = None
    last_offset = 0
    for ts in io_util.timeserie_gz_read_iterator(entry.filepath):
      if ts.metadata.ref_secs: ref_secs = ts.metadata.ref_secs
      last_offset = ts.offset_millis[-1]
    entry.end_secs = int(last_offset/1000) + ref_secs
  return (entry.start_secs, entry.end_secs, entry.filepath)

def add_timeserie_files_to_repo(conf, replay_repo):
  timeserie_repo = io_util.parse_from_gz_file(conf.timeserie_repo, timeserie_pb2.TimeserieRepo())
  if len(timeserie_repo.entries) == 0:
    logging.warning("File %r does not exist, skipping timeseries", conf.timeserie_repo)
    return timeserie_repo

  bound_path_tuples = sorted( extract_timeserie_time_bounds(e) for e in timeserie_repo.entries )
  sorted_start = [ t[0] for t in bound_path_tuples ]
  for replay in replay_repo.replays:
    if replay.timeserie: continue
    idx = max(0, bisect.bisect_left(sorted_start, replay.start_secs) - 1)
    if is_inside(replay, bound_path_tuples[idx][0], bound_path_tuples[idx][1]):
      replay.timeserie = bound_path_tuples[idx][2]
    elif is_partially_inside(replay, bound_path_tuples[idx][0], bound_path_tuples[idx][1]):
      logging.warning("Mismatched time bounds between timeserie and replay: %r / %r",
                      bound_path_tuples, replay)
      replay.timeserie = bound_path_tuples[idx][2]
    else:
      logging.warning("No timeserie found for replay: %r", replay)
  return timeserie_repo

def parse_replay_files(conf, replay_repo):
  if not conf.do_parse: return
  # We import inside the function scope to avoid executing the module code unless
  # explicetly requested
  import unsafe_game_parser
  workers = max(1, int(os.cpu_count() / 2))
  parser_inputs = [ util.ParserInput(idx, conf, r.filepath, r.start_secs)
                    for idx,r in enumerate(replay_repo.replays)
                    if not r.details or conf.rebuild ]
  with multiprocessing.Pool(workers) as pool:
    # see util.ParserInput, util.ParserResult
    parser_results = pool.map(unsafe_game_parser.write_game_details,
                              parser_inputs)

  fiascos = [ True for r in parser_results if not r.ok ]
  if fiascos:
    logging.warning("Failed to parse %d replays", len(fiascos))

  for parser_in,parser_out in zip(parser_inputs, parser_results):
    if not parser_out.ok: continue
    idx = parser_in.replay_idx
    replay = replay_repo.replays[idx]
    replay.details = parser_out.filepath
    replay.start_secs = max(replay.start_secs,
                            replay.end_secs - parser_out.game_duration_secs)

def build_and_write_replay_repo(conf):
  replay_repo = load_repo_from_file(conf)
  find_replay_files(conf, replay_repo)
  parse_replay_files(conf, replay_repo)
  timeserie_repo = add_timeserie_files_to_repo(conf, replay_repo)
  io_util.serialize_to_gz_file(conf.replay_repo, replay_repo)
  return replay_repo, timeserie_repo

def main():
  conf = util.init_conf()
  util.init_logging(conf)
  build_and_write_replay_repo(conf)

if __name__ == '__main__':
  main()

