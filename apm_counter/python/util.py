import os
import sys
import argparse
import logging
import collections

ParserResult = collections.namedtuple('ParserResult', ['filepath', 'game_duration_secs'])

def init_conf(argv=None):
  parser = argparse.ArgumentParser("apm counter")
  parser.add_argument("subcommand", nargs='+')
  parser.add_argument("--log_lvl", default='DEBUG')
  parser.add_argument("--steam_dir", default='')
  parser.add_argument("--aoe2_usr_dir", default='')
  parser.add_argument("--ts_root", default='')
  parser.add_argument("--rebuild", action='store_true', default=False)
  parser.add_argument("--do_parse", action='store_true', default=False)
  conf = parser.parse_args(argv or sys.argv)
  conf.aoe2_replay_dir = os.path.join(conf.steam_dir, conf.aoe2_usr_dir)
  conf.timeserie_repo = os.path.join(conf.ts_root, "timeserie_repo.pb.gz")
  conf.replay_repo = os.path.join(conf.ts_root, "replay_repo.pb.gz")
  return conf

def init_logging(conf):
  levels = [logging.DEBUG, logging.INFO, logging.WARNING, logging.ERROR]
  str_to_lvl = { logging.getLevelName(l).lower():l for l in levels }
  logging.basicConfig(level=levels[conf.log_lvl.lower()],
                      format='[%(levelname)s] %(name)s::%(funcName)s  %(message)s')

