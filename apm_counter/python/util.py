import os
import sys
import argparse
import logging
import collections

import replay_pb2

ParserInput = collections.namedtuple('ParserInput', ['replay_idx', 'conf', 'filepath', 'start_secs'])
ParserResult = collections.namedtuple('ParserResult', ['ok', 'filepath', 'game_duration_secs'])

def init_conf(argv=None):
  parser = argparse.ArgumentParser("apm counter")
  parser.add_argument("subcommand", nargs='*')
  parser.add_argument("--aoe2_usr_dir", default='')
  parser.add_argument("--do_parse", action='store_true', default=False)
  parser.add_argument("--log_lvl", default='DEBUG')
  parser.add_argument("--player", default='')
  parser.add_argument("--rebuild", action='store_true', default=False)
  parser.add_argument("--replay_file", default='')
  parser.add_argument("--steam_dir", default='')
  parser.add_argument("--ts_root", default='')
  conf = parser.parse_args(argv if argv != None else sys.argv[1:])
  conf.usage = parser.format_usage()
  add_derived_fields(conf)
  return conf

def add_derived_fields(conf):
  conf.aoe2_replay_dir = os.path.join(conf.steam_dir, conf.aoe2_usr_dir)
  conf.timeserie_repo = os.path.join(conf.ts_root, "timeserie_repo.pb.gz")
  conf.replay_repo = os.path.join(conf.ts_root, "replay_repo.pb.gz")

def init_logging(conf):
  levels = [logging.DEBUG, logging.INFO, logging.WARNING, logging.ERROR]
  str_to_lvl = { logging.getLevelName(l).lower():l for l in levels }
  logging.basicConfig(level=str_to_lvl[conf.log_lvl.lower()],
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')
  mpl_logger = logging.getLogger('matplotlib')
  mpl_logger.setLevel(logging.WARNING)
  logging.info("Running with conf:\n%s", "\n".join( "  %s: %r" % (k,v) for k,v in conf.__dict__.items() ))

def get_player(game_details, name):
  return next( p for p in game_details.players if p.name == name)

def get_tech_age_research_times(game_details, name):
  player = get_player(game_details, name)
  techs = [replay_pb2.FEUDAL_AGE, replay_pb2.CASTLE_AGE, replay_pb2.IMPERIAL_AGE]
  age_millis = { t:0 for t in techs }
  for action in player.actions:
    if action.type != replay_pb2.RESEARCH or action.tech_id not in techs: continue
    real_millis = int(action.offset_millis / game_details.game_speed)
    age_millis[action.tech_id] = real_millis
  return age_millis

def replay_to_timeserie_millis_offset(replay, metadata, offset):
  abs_millis = replay.start_secs * 1000 + offset
  return abs_millis - metadata.ref_secs * 1000

