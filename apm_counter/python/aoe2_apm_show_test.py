import os
import matplotlib
import pandas as pd
import tempfile
import time
import unittest

import aoe2_apm_show
import io_util
import test_case_base
import util

class TestAoeApmShow(test_case_base.TestCaseBase):

  @classmethod
  def setUpClass(cls):
    matplotlib.use('SVG')
    pd.set_option('plotting.backend', 'matplotlib')

  def test_plot_last_replay_in_repo(self):
    ts,filepath = self.create_test_timeserie(33, 66*1000) 
    replay_repo = self.create_test_replay_repo([0, 33], [11, 99], ['rec1', 'rec2'])
    replay = replay_repo.replays[1]
    replay.timeserie = filepath
    self.conf.subcommand = ['plot_last_replay']
    io_util.serialize_to_gz_file(self.conf.replay_repo, replay_repo)
    aoe2_apm_show.plot_last_replay_in_repo(self.conf)

  def test_plot_replay_by_idx(self):
    ts,filepath = self.create_test_timeserie(33, 66*1000) 
    replay_repo = self.create_test_replay_repo([0, 33], [11, 99], ['rec1', 'rec2'])
    replay = replay_repo.replays[1]
    replay.timeserie = filepath
    self.conf.subcommand = ['plot_replay', '1']
    io_util.serialize_to_gz_file(self.conf.replay_repo, replay_repo)
    aoe2_apm_show.plot_replay_by_idx(self.conf)

  def test_plot_most_recent_ts_in_repo(self):
    ts,filepath = self.create_test_timeserie(33, 66*1000) 
    ts_repo = self.create_test_timeserie_repo([0, 33], [11, 99], ['does_not_exist', filepath])
    self.conf.subcommand = ['plot_last_ts']
    aoe2_apm_show.plot_most_recent_ts_in_repo(self.conf)
    
  def test_plot_ts_file(self):
    ts,filepath = self.create_test_timeserie(33, 66*1000) 
    self.conf.subcommand = ['plot_ts', filepath]
    axes = aoe2_apm_show.plot_ts_file(self.conf)
    figpath = os.path.join(self.conf.ts_root, 'graph.svg')
    axes.figure.savefig(figpath)
    self.assertTrue(os.path.exists(figpath))
    
  def test_short_listing_format(self):
    now_secs = 1613324411
    replay_repo = self.create_test_replay_repo([now_secs], [now_secs + 3600], ['record_file'])
    line = aoe2_apm_show.short_listing_format(self.conf, 1, replay_repo.replays[0])
    self.assertEqual('[01] 2021/02/14 18h40 (1:00:00) timeserie=False parsed=False', line)

  def test_list_replay_in_repo(self):
    self.conf.subcommand = ['list']
    replay_repo = self.create_test_replay_repo([12, 24, 86], [20, 44, 111], ['rec1', 'rec2', 'rec3'])
    aoe2_apm_show.list_replay_in_repo(self.conf)

  def test_find_new_replay_and_ts(self):
    self.conf.subcommand = ['find_new']
    self.conf.do_parse = False
    aoe2_apm_show.find_new_replay_and_ts(self.conf)
    
if __name__ == '__main__':
  unittest.main()

