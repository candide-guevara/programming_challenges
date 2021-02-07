import datetime as dt
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

# Be careful it is a trap !
# Comparison between datetime64 of different units is undefined
# np.datetime64(333, 'ms') > np.datetime64(np.iinfo(np.int64).max, 's' is True !
def millis_to_dt(millis, ts_start=None):
  if not ts_start:
    ts_start = np.datetime64(0, 'ms')
  return ts_start + np.timedelta64(millis, 'ms')

def millis_to_delta(millis):
  return np.timedelta64(millis, 'ms')

def dt_delta(upper_dt, lower_dt):
  if upper_dt < lower_dt: return np.timedelta64(0, 'ms')
  return upper_dt - lower_dt

def adjust_bounds_to_time(index, ts_start, lower_dt, upper_dt):
  lower_delta = dt_delta(lower_dt, ts_start)
  upper_delta = dt_delta(upper_dt, ts_start)
  lower,upper = np.searchsorted(index, [lower_delta, upper_delta])
  return max(0,lower), min(upper,len(index))

def metadata_start_dt(ts):
  return millis_to_dt(ts.metadata.ref_secs * 1000)

def read_df_from_timeserie_gz_between(filepath, lower_dt, upper_dt):
  cur_size = 0
  read_it = timeserie_gz_read_iterator(filepath)
  index,data = allocate_and_copy(20000, None, None)
  ts_start = None

  for ts in read_it:
    if ts.metadata:
      ts_start = metadata_start_dt(ts)
    if (cur_size + len(ts.offset_millis)) > len(index):
      index,data = allocate_and_copy(cur_size * 2, index, data)

    if millis_to_dt(ts.offset_millis[0] , ts_start) > upper_dt: break
    if millis_to_dt(ts.offset_millis[-1], ts_start) < lower_dt: continue

    write_ts_to_np_array(cur_size, index, data, ts)
    cur_size += len(ts.offset_millis)
  lower,upper = adjust_bounds_to_time(index[0:cur_size], ts_start, lower_dt, upper_dt)
  return df_from_nparray(index[lower:upper], data[lower:upper])

def read_df_from_timeserie_gz(filepath):
  lower_dt = millis_to_dt(0)
  upper_dt = millis_to_dt(np.iinfo(np.int64).max)
  return read_df_from_timeserie_gz_between(filepath, lower_dt, upper_dt)

