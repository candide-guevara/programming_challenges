import gzip
import numpy as np
import pandas as pd
import struct

import timeserie_pb2

DATA_COLS = ['kbd', 'mse', 'btn']

def timeserie_gz_read_iterator(filepath):
  with gzip.open(filepath) as fobj:
    yield from timeserie_read_iterator(fobj)

def timeserie_read_iterator(fobj):
  size_buf = bytearray(4)
  buf = memoryview(bytearray(1024**2))
  ts = timeserie_pb2.Timeserie()

  while True:
    if fobj.readinto(size_buf) != len(size_buf): return
    size = struct.unpack('<I', size_buf)[0]
    if size > len(buf):
      buf = memoryview(bytearray(size))
    mv = buf[0:size]
    fobj.readinto(mv)
    ts.ParseFromString(mv)
    yield ts
  
def timeserie_gz_write(filepath, ts_list):
  with gzip.open(filepath, mode='wb') as fobj:
    for ts in ts_list: timeserie_write(fobj, ts)

# Did not find a way to preallocate buffers in the protobuf API. 
def timeserie_write(fobj, ts):
  buf = ts.SerializeToString()
  fobj.write(struct.pack('<I', len(buf)))
  fobj.write(buf)
  
def write_ts_to_np_array(offset, index, data, ts):
  ts_len = len(ts.offset_millis)
  if not ts_len: return
  end = ts_len + offset
  index[offset:end]   = ts.offset_millis
  data[offset:end, 0] = ts.kbd_count
  data[offset:end, 1] = ts.mse_count
  data[offset:end, 2] = ts.btn_count

def allocate_and_copy(size, ro_index, ro_data):
  index = np.ndarray((size,), np.timedelta64(1, 'ms'))
  data = np.ndarray((size,len(DATA_COLS)), np.uint32)
  if ro_index:
    index[0:len(ro_index)] = ro_index
  if ro_data:
    data[0:len(ro_data)] = ro_data
  return index,data

def df_from_nparray(index, data):
  return pd.DataFrame(data=data, index=index, columns=DATA_COLS)

def read_df_from_timeserie_gz(filepath):
  cur_size = 0
  read_it = timeserie_gz_read_iterator(filepath)
  index,data = allocate_and_copy(20000, None, None)
  for ts in read_it:
    if (cur_size + len(ts.offset_millis)) > len(index):
      index,data = allocate_and_copy(20000, index, data)
    write_ts_to_np_array(cur_size, index, data, ts)
    cur_size += len(ts.offset_millis)
  return df_from_nparray(index[0:cur_size], data[0:cur_size])

