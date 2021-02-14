import os
import tempfile
import time
import unittest

import aoe2_replay_repo_builder
import io_util
import replay_pb2
import timeserie_pb2
import util

class TestCaseBase(unittest.TestCase):

  def create_test_timeserie_repo(self, start_list, end_list, name_list):
    repo = timeserie_pb2.TimeserieRepo()
    for idx,name in enumerate(name_list):
      entry = repo.entries.add()
      entry.filepath = os.path.join(self.conf.ts_root, name)
      entry.start_secs = start_list[idx]
      if end_list[idx]: entry.end_secs = end_list[idx]
    io_util.serialize_to_gz_file(self.conf.timeserie_repo, repo)
    return repo

  def create_test_game_details(self):
    game_details = replay_pb2.GameDetails()
    game_details.map_name = 'arabia'
    game_details.map_size = 200
    game_details.game_speed = 2.0
    game_details.duration_millis = 600*1000

    random_player = game_details.players.add()
    my_player = game_details.players.add()
    my_player.name = self.conf.player
    
    feudal = my_player.actions.add()
    castle = my_player.actions.add()
    imperial = my_player.actions.add()

    feudal.offset_millis = 60*1000
    castle.offset_millis = 120*1000
    imperial.offset_millis = 200*1000

    feudal.type = replay_pb2.RESEARCH
    castle.type = replay_pb2.RESEARCH
    imperial.type = replay_pb2.RESEARCH

    feudal.tech_id = replay_pb2.FEUDAL_AGE
    castle.tech_id = replay_pb2.CASTLE_AGE
    imperial.tech_id = replay_pb2.IMPERIAL_AGE

    filepath = os.path.join(self.conf.ts_root, 'game_details.pb.gz')
    io_util.serialize_to_gz_file(filepath, game_details)
    return game_details,filepath

  def create_test_replay_repo(self, start_list, end_list, name_list):
    repo = replay_pb2.ReplayRepo()
    for idx,name in enumerate(name_list):
      entry = repo.replays.add()
      entry.filepath = os.path.join(self.conf.aoe2_replay_dir, name)
      entry.start_secs = start_list[idx]
      entry.end_secs = end_list[idx]
    io_util.serialize_to_gz_file(self.conf.replay_repo, repo)
    return repo

  def create_test_timeserie(self, start_secs, last_offset):
    expect_ts = timeserie_pb2.Timeserie()
    expect_ts.metadata.ref_secs = start_secs
    expect_ts.metadata.ref_nanos = 0
    expect_ts.offset_millis.extend(list(range(0, last_offset+1, 1000)))
    expect_ts.kbd_count.extend    (list(range(0, last_offset+1, 1000)))
    expect_ts.mse_count.extend    (list(range(0, last_offset+1, 1000)))
    expect_ts.btn_count.extend    (list(range(0, last_offset+1, 1000)))
    with tempfile.NamedTemporaryFile(dir=self.conf.ts_root, prefix='timeserie.',
                                     suffix='.pb.gz', delete=False) as ts_file: pass
    io_util.timeserie_gz_write(ts_file.name, [expect_ts])
    return expect_ts, ts_file.name

  def setUp(self):
    self.test_dir = tempfile.TemporaryDirectory()
    self.conf = util.init_conf([])
    self.conf.steam_dir = self.test_dir.name
    self.conf.ts_root = self.test_dir.name
    self.conf.aoe2_usr_dir = 'replays'
    self.conf.player = 'mr_monkey'
    os.mkdir(os.path.join(self.test_dir.name, self.conf.aoe2_usr_dir))
    util.add_derived_fields(self.conf)
    util.init_logging(self.conf)

  def tearDown(self):
    self.test_dir.cleanup()


