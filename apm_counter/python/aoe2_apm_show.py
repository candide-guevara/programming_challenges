import datetime as dt
import logging
import os

import aoe2_replay_repo_builder
import io_util
import util

import replay_pb2
import timeserie_pb2

# Lazy import to allow setting the backend for unittests
def lazy_import_matplotlib():
  global matplotlib, plt
  import matplotlib
  import matplotlib.pyplot as plt

def find_new_replay_and_ts(conf):
  replay_repo, timeserie_repo = aoe2_replay_repo_builder.build_and_write_replay_repo(conf)
  logging.info("%d replays in repo\n%d timeseries in repo",
               len(replay_repo.replays), len(timeserie_repo.entries))

def short_listing_format(conf, idx, replay):
  str_date = dt.datetime.fromtimestamp(replay.start_secs).strftime("%Y/%m/%d %H:%M")
  duration = dt.timedelta(seconds=(replay.end_secs - replay.start_secs))
  return "[%02d] %s (%s) timeserie=%r parsed=%r" % (
         idx, str_date, duration, replay.timeserie or False, replay.details or False)

# https://matplotlib.org/stable/gallery/units/artist_tests.html#sphx-glr-gallery-units-artist-tests-py
# https://stackoverflow.com/questions/26700598/matplotlib-showing-x-tick-labels-overlapping
def plot_ts(conf, ts):
  axes = ts.plot()
  return axes

def plot_replay(conf, replay):
  if not replay.timeserie:
    raise Exception("No timeserie for replay %s" % replay)
  ts,metadata = io_util.read_df_from_timeserie_gz_between(replay.timeserie,
                                                 io_util.secs_to_dt(replay.start_secs),
                                                 io_util.secs_to_dt(replay.end_secs))
  axes = plot_ts(conf, ts)
  if replay.details:
    game_details = io_util.parse_from_gz_file(replay.details, replay_pb2.GameDetails())
    age_millis = util.get_tech_age_research_times(game_details, conf.player)
    for offset in age_millis.values():
      real_offset = util.replay_to_timeserie_millis_offset(replay, metadata, offset)
      line = matplotlib.lines.Line2D([offset,offset], [0,1000],
                                    lw=2, color='black', axes=axes)
      axes.add_artist(line)
  plt.show()
  return axes

def plot_ts_file(conf):
  if len(conf.subcommand) < 2:
    raise Exception("plot_ts needs filepath")
  ts,_ = io_util.read_df_from_timeserie_gz(conf.subcommand[1])
  axes = plot_ts(conf, ts)
  plt.show()
  return axes

def plot_most_recent_ts_in_repo(conf):
  timeserie_repo = io_util.parse_from_gz_file(conf.timeserie_repo, timeserie_pb2.TimeserieRepo())
  lastest_entry = max(( e for e in timeserie_repo.entries ), key=lambda e:e.start_secs)
  ts,_ = io_util.read_df_from_timeserie_gz(lastest_entry.filepath)
  axes = plot_ts(conf, ts)
  plt.show()
  return axes

def plot_replay_by_idx(conf):
  if len(conf.subcommand) < 2:
    raise Exception("plot_ts needs index")
  replay_repo = io_util.parse_from_gz_file(conf.replay_repo, replay_pb2.ReplayRepo())
  plot_replay(conf, replay_repo.replays[int(conf.subcommand[1])])

def plot_last_replay_in_repo(conf):
  replay_repo = io_util.parse_from_gz_file(conf.replay_repo, replay_pb2.ReplayRepo())
  lastest_replay = max(( r for r in replay_repo.replays ), key=lambda r:r.start_secs)
  plot_replay(conf, lastest_replay)

def list_replay_in_repo(conf):
  is_short_listing = False
  if len(conf.subcommand) > 1:
    is_short_listing = 'short' == conf.subcommand[1]
  replay_repo = io_util.parse_from_gz_file(conf.replay_repo, replay_pb2.ReplayRepo())
  
  if not is_short_listing:
    logging.info("Replay repo:\n%s", replay_repo)
  else:
    lines = [ short_listing_format(conf, i, r) for i,r in enumerate(replay_repo.replays) ]
    logging.info("Replay repo:\n%s", "\n".join(lines))

def main():
  conf = util.init_conf()
  util.init_logging(conf)
  if not conf.subcommand:
    logging.error("No subcommand specified\n%s", conf.usage)
    return
  subcmd_func = {
    'find_new' : find_new_replay_and_ts,
    'list' : list_replay_in_repo,
    'plot_ts' : plot_ts_file,
    'plot_last_ts' : plot_most_recent_ts_in_repo,
    'plot_replay' : plot_replay_by_idx,
    'plot_last_replay' : plot_last_replay_in_repo,
  }
  lazy_import_matplotlib()
  subcmd_func[conf.subcommand[0]](conf)

if __name__ == '__main__':
  main()


