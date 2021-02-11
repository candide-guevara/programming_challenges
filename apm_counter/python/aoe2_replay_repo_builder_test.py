import os
import tempfile
import time
import unittest

import aoe2_replay_repo_builder
import io_util
import replay_pb2
import timeserie_pb2
import util

class TestAoeReplayRepoBuilder(unittest.TestCase):

  @classmethod
  def setUpClass(cls):
    pass

  def create_dummy_record_files(self, count):
    records = []
    for i in range(count):
      r_file = tempfile.mkstemp(suffix='.aoe2record', prefix='replay_',
                                dir=self.conf.aoe2_replay_dir)
      os.close(r_file[0])
      records.append(r_file[1])
    return records

  def create_test_timeserie_repo(self, start_list, end_list, name_list):
    repo = timeserie_pb2.TimeserieRepo()
    for idx,name in enumerate(name_list):
      entry = repo.entries.add()
      entry.filepath = os.path.join(self.conf.ts_root, name)
      entry.start_secs = start_list[idx]
      if end_list[idx]: entry.end_secs = end_list[idx]
    io_util.serialize_to_gz_file(self.conf.timeserie_repo, repo)
    return repo

  def create_test_replay_repo(self, start_list, end_list, name_list):
    repo = replay_pb2.ReplayRepo()
    for idx,name in enumerate(name_list):
      entry = repo.replays.add()
      entry.filepath = os.path.join(self.conf.aoe2_replay_dir, name)
      entry.start_secs = start_list[idx]
      entry.end_secs = end_list[idx]
    io_util.serialize_to_gz_file(self.conf.replay_repo, repo)
    return repo

  def create_test_timeserie(self, start_secs, last_offset, filename):
    expect_ts = timeserie_pb2.Timeserie()
    expect_ts.metadata.ref_secs = start_secs
    expect_ts.metadata.ref_nanos = 0
    expect_ts.offset_millis.extend([0, last_offset])
    expect_ts.kbd_count.extend    ([0, last_offset])
    expect_ts.mse_count.extend    ([0, last_offset])
    expect_ts.btn_count.extend    ([0, last_offset])
    filepath = os.path.join(self.conf.ts_root, filename)
    io_util.timeserie_gz_write(filepath, expect_ts)
    return expect_ts

  def setUp(self):
    self.test_dir = tempfile.TemporaryDirectory()
    self.conf = util.init_conf([])
    self.conf.steam_dir = self.test_dir.name
    self.conf.ts_root = self.test_dir.name
    self.conf.aoe2_usr_dir = 'replays'
    os.mkdir(os.path.join(self.test_dir.name, self.conf.aoe2_usr_dir))
    util.add_derived_fields(self.conf)

  def tearDown(self):
    self.test_dir.cleanup()

  def test_load_repo_from_file(self):
    self.conf.rebuild = True
    ini_repo = self.create_test_replay_repo([0], [0], ['rec'])
    repo = aoe2_replay_repo_builder.load_repo_from_file(self.conf)
    self.assertEqual(0, len(repo.replays))

    self.conf.rebuild = False
    ini_repo = self.create_test_replay_repo([0], [0], ['rec'])
    repo = aoe2_replay_repo_builder.load_repo_from_file(self.conf)
    self.assertEqual(str(ini_repo), str(repo))

  def test_find_replay_files(self):
    unix_secs = int(time.time())
    records = self.create_dummy_record_files(3)
    repo = replay_pb2.ReplayRepo()
    aoe2_replay_repo_builder.find_replay_files(self.conf, repo)
    self.assertEqual(len(records), len(repo.replays))

  def test_parse_replay_files(self):
    self.conf.do_parse = False
    repo = replay_pb2.ReplayRepo()
    aoe2_replay_repo_builder.parse_replay_files(self.conf, repo)
    self.assertEqual(0, len(repo.replays))

  def test_add_timeserie_files_to_repo(self):
    #s_1 = self.create_test_timeserie(10, 50, 'ts1.pb.gz')
    #ts_2 = self.create_test_timeserie(80, 50, 'ts2.pb.gz')
    ts_repo = self.create_test_timeserie_repo([10, 80], [60, 130], ['ts1.pb.gz', 'ts2.pb.gz'])
    replay_repo = self.create_test_replay_repo([12, 24, 86], [20, 44, 111], ['rec1', 'rec2', 'rec3'])
    aoe2_replay_repo_builder.add_timeserie_files_to_repo(self.conf, replay_repo)
    self.assertEqual(replay_repo.replays[0].timeserie, ts_repo.entries[0].filepath)
    self.assertEqual(replay_repo.replays[1].timeserie, ts_repo.entries[0].filepath)
    self.assertEqual(replay_repo.replays[2].timeserie, ts_repo.entries[1].filepath)

if __name__ == '__main__':
  unittest.main()

