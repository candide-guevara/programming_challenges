import numpy as np
import pandas as pd
import tempfile
import unittest

import io_util
import timeserie_pb2

class TestTimeserieIO(unittest.TestCase):

  def create_test_timeserie(self):
    expect_ts = timeserie_pb2.Timeserie()
    expect_ts.metadata.ref_secs = 333
    expect_ts.metadata.ref_nanos = 666
    expect_ts.metadata.period_millis = 500
    expect_ts.offset_millis.extend(list(range(1, 11, 1)))
    expect_ts.kbd_count.extend    (list(range(1, 21, 2)))
    expect_ts.mse_count.extend    (list(range(1, 21, 2)))
    expect_ts.btn_count.extend    (list(range(1, 21, 2)))
    return expect_ts

  def create_test_gz_and_expected_ts_list(self, batch_len):
    model_ts = self.create_test_timeserie()
    last_offset = 0
    ts_list = []

    # Convoluted way since mktemp is deprecated
    with tempfile.NamedTemporaryFile(delete=False) as tmp_file: pass
    for i in range(3):
      ts = timeserie_pb2.Timeserie()
      ts.CopyFrom(model_ts)
      shifted_millis = [ last_offset+t for t in ts.offset_millis ]
      ts.offset_millis[:] = shifted_millis
      last_offset = shifted_millis[-1]
      ts_list.append(ts)

    io_util.timeserie_gz_write(tmp_file.name, ts_list)
    return tmp_file.name, ts_list

  def test_write_read_pb_stream(self):
    filepath,ts_list = self.create_test_gz_and_expected_ts_list(3)
    read_it = io_util.timeserie_gz_read_iterator(filepath)
    for idx,tup in enumerate(zip(read_it, ts_list)):
      ts,expect = tup
      expect_str = str(expect)
      self.assertEqual(str(ts), expect_str)
    self.assertEqual(idx+1, len(ts_list))

  def test_read_df_from_timeserie_gz_between(self):
    filepath,ts_list = self.create_test_gz_and_expected_ts_list(5)
    ts_start = io_util.metadata_start_dt(ts_list[0])
    batch_len = len(ts_list[0].offset_millis)
    lower_dt = io_util.millis_to_dt(3, ts_start)
    upper_dt = io_util.millis_to_dt(3 + batch_len*2, ts_start)
    last_dt = upper_dt - io_util.millis_to_delta(1)
    df,_ = io_util.read_df_from_timeserie_gz_between(filepath, lower_dt, upper_dt)
    self.assertEqual(df.shape, (batch_len*2, len(io_util.DATA_COLS)))
    self.assertEqual(df.index[0], io_util.dt_delta(lower_dt, ts_start))
    self.assertEqual(df.index[-1], io_util.dt_delta(last_dt, ts_start))

  def test_read_df_from_timeserie_gz(self):
    filepath,ts_list = self.create_test_gz_and_expected_ts_list(3)
    total_len = sum( len(ts.offset_millis) for ts in ts_list )
    df,_ = io_util.read_df_from_timeserie_gz(filepath)

    self.assertEqual(df.shape, (total_len, len(io_util.DATA_COLS)))
    offset = 0
    for ts in ts_list:
      ts_len = len(ts.offset_millis)
      offset_millis = [ io_util.millis_to_delta(t) for t in ts.offset_millis ]
      self.assertEqual(list(df.index[offset:offset+ts_len]), offset_millis)
      self.assertEqual(df.iloc[offset:offset+ts_len,0].to_list(), ts.kbd_count)
      self.assertEqual(df.iloc[offset:offset+ts_len,1].to_list(), ts.mse_count)
      self.assertEqual(df.iloc[offset:offset+ts_len,2].to_list(), ts.btn_count)
      offset += ts_len

  def test_write_ts_to_np_array(self):
    ts = self.create_test_timeserie()
    ts_len = len(ts.offset_millis)
    index,data = io_util.allocate_and_copy(ts_len, None, None)
    io_util.write_ts_to_np_array(0, index, data, ts)
    self.assertEqual(list(index), ts.offset_millis)
    self.assertEqual(list(data[:,0]), ts.kbd_count)
    self.assertEqual(list(data[:,1]), ts.mse_count)
    self.assertEqual(list(data[:,2]), ts.btn_count)

  def test_zero_copy_df_creation(self):
    index,data = io_util.allocate_and_copy(10, None, None)
    df = io_util.df_from_nparray(index[1:6], data[1:6])
    self.assertEqual(df.shape, (5, len(io_util.DATA_COLS)))
    index[1] = 666
    data[1,0] = 666
    self.assertEqual(df.iloc[0,0], data[1,0])
    #index is always copied
    #self.assertEqual(df.index[0], index[0])

if __name__ == '__main__':
  unittest.main()

