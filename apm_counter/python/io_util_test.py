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
    expect_ts.offset_millis.extend([1,2,3,4])
    expect_ts.kbd_count.extend([1,2,3,4])
    expect_ts.mse_count.extend([1,2,3,4])
    expect_ts.btn_count.extend([1,2,3,4])
    return expect_ts

  def create_test_gz_and_expected_ts_list(self):
    expect_ts = self.create_test_timeserie()
    ts_list = [expect_ts, expect_ts, expect_ts]

    # Convoluted way since mktemp is deprecated
    with tempfile.NamedTemporaryFile(delete=False) as tmp_file: pass
    io_util.timeserie_gz_write(tmp_file.name, ts_list)
    return tmp_file.name, ts_list

  def test_write_read_pb_stream(self):
    filepath,ts_list = self.create_test_gz_and_expected_ts_list()
    expect_str = str(ts_list[0])
    for idx,ts in enumerate(io_util.timeserie_gz_read_iterator(filepath)):
      self.assertEqual(str(ts), expect_str)
    self.assertEqual(idx+1, len(ts_list))

  def test_read_df_from_timeserie_gz(self):
    filepath,ts_list = self.create_test_gz_and_expected_ts_list()
    total_len = sum( len(ts.offset_millis) for ts in ts_list )
    df = io_util.read_df_from_timeserie_gz(filepath)

    self.assertEqual(df.shape, (total_len, len(io_util.DATA_COLS)))
    offset = 0
    for ts in ts_list:
      ts_len = len(ts.offset_millis)
      offset_millis = np.array(ts.offset_millis, np.timedelta64(1, 'ms'))
      self.assertEqual(list(df.index[offset:offset+ts_len]), list(offset_millis))
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
    df = io_util.df_from_nparray(index[0:5], data[0:5])
    self.assertEqual(df.shape, (5, len(io_util.DATA_COLS)))
    index[0] = 666
    data[0,0] = 666
    self.assertEqual(df.iloc[0,0], data[0,0])
    #index is always copied
    #self.assertEqual(df.index[0], index[0])

if __name__ == '__main__':
  unittest.main()

