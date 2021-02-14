import os
import tempfile
import time
import unittest

import aoe2_replay_repo_builder
import io_util
import replay_pb2
import test_case_base
import timeserie_pb2
import util

class TestAoeReplayRepoBuilder(test_case_base.TestCaseBase):

  def create_dummy_record_files(self, count):
    records = []
    for i in range(count):
      r_file = tempfile.mkstemp(suffix='.aoe2record', prefix='replay_',
                                dir=self.conf.aoe2_replay_dir)
      os.close(r_file[0])
      records.append(r_file[1])
    return records

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

  def test_extract_timeserie_time_bounds(self):
    ts,filepath = self.create_test_timeserie(33, 66*1000) 
    ts_repo = self.create_test_timeserie_repo([33], [99], [filepath])
    entry = ts_repo.entries[0]
    start,end,fp = aoe2_replay_repo_builder.extract_timeserie_time_bounds(entry)
    self.assertEqual(start, 33)
    self.assertEqual(end, 99)

    entry.end_secs = 0
    start,end,fp = aoe2_replay_repo_builder.extract_timeserie_time_bounds(entry)
    self.assertEqual(start, 33)
    self.assertEqual(end, 99)
    self.assertEqual(ts_repo.entries[0].end_secs, 99)

  def test_add_timeserie_files_to_repo(self):
    #ts_1 = self.create_test_timeserie(10, 50*1000, 'ts1.pb.gz')
    #ts_2 = self.create_test_timeserie(80, 50*1000, 'ts2.pb.gz')
    ts_repo = self.create_test_timeserie_repo([10, 80], [60, 130], ['ts1.pb.gz', 'ts2.pb.gz'])
    replay_repo = self.create_test_replay_repo([12, 24, 86], [20, 44, 111], ['rec1', 'rec2', 'rec3'])
    aoe2_replay_repo_builder.add_timeserie_files_to_repo(self.conf, replay_repo)
    self.assertEqual(replay_repo.replays[0].timeserie, ts_repo.entries[0].filepath)
    self.assertEqual(replay_repo.replays[1].timeserie, ts_repo.entries[0].filepath)
    self.assertEqual(replay_repo.replays[2].timeserie, ts_repo.entries[1].filepath)

if __name__ == '__main__':
  unittest.main()

