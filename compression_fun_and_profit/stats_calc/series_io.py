import pandas as pd, numpy as np, gzip
import common, pickle, struct
logger = common.logging.getLogger(__name__)

class DFormat:
  RAW,NORMAL,DELTA = 0,1,2

class FFormat:
  UNKNOWN = 0

class MetaBase:
  @classmethod
  def unpack_from_file(klass, fileobj):
    buf = fileobj.read(klass.size)
    return klass.unpack_from(buf)

class FileHeader(MetaBase):
  __slots__ = ('fformat', 'dformat', 'count')
  fmt = '=hhI'
  size = 8

  def __init__(self, fformat=None, dformat=None, count=0):
    self.fformat = fformat
    self.dformat = dformat
    self.count = count

  @classmethod
  def unpack_from(klass, buf, offset=0):
    meta = klass()
    meta.fformat, meta.dformat, meta.count = \
      struct.unpack_from(klass.fmt, buf, offset)
    return meta

  def pack(self):
    return struct.pack(FileHeader.fmt, 
      self.fformat, self.dformat, self.count)

class CompMetadata(MetaBase):
  __slots__ = ('sid', 'min', 'max', 'start')
  fmt = '=QddI'
  size = 28

  @classmethod
  def unpack_from(klass, buf, offset=0):
    meta = klass()
    meta.sid, meta.min, meta.max, meta.start = \
      struct.unpack_from(klass.fmt, buf, offset)
    return series

  def pack(self):
    return struct.pack(CompMetadata.fmt, 
      self.sid, self.min, self.max, self.start)

class SerieMetadata(MetaBase):
  __slots__ = ('sid', 'min', 'max', 'start', 'count')
  fmt = '=QddII'
  size = 32

  @classmethod
  def unpack_from(klass, buf, offset=0):
    meta = klass()
    meta.sid, meta.min, meta.max, meta.start, meta.count = \
      struct.unpack_from(klass.fmt, buf, offset)
    return meta

  def pack(self):
    return struct.pack(SerieMetadata.fmt, 
      self.sid, self.min, self.max, self.start, self.count)
    

class Series:
  def __init__(self, dformat):
    self.meta = []
    self.data = []
    self.count = 0
    self.dformat = dformat

  def add(self, meta, data):
    self.meta.append(meta)
    self.data.append(data)
    self.count += 1

  def add_from_np_maarray(self, sid, start, data):
    meta = SerieMetadata()
    meta.sid = sid_str_to_num(sid)
    meta.start = start
    meta.min = data.min()
    meta.max = data.max()
    meta.count = len(data)
    self.add(meta, data)

def sid_str_to_num(sid):
  return int(sid[2:])

# array of dataframes
# each dataframe contains several series (df multindex on security id)
def load_from_df_chunks(infile, colname):
  series = Series(DFormat.RAW)
  try:
    with gzip.open(infile, 'rb') as fileobj:
      while True:
        df_multi = pickle.load(fileobj)
        for sid in df_multi.index.levels[0]:
          df = df_multi.loc[sid]
          non_na_date = df[colname].last_valid_index()
          if non_na_date == None: continue

          start_pos = df.index.get_loc(non_na_date)
          df_valid_slice = df.iloc[:start_pos+1]
          data = np.ma.masked_invalid(df_valid_slice[colname].values)
          series.add_from_np_maarray(sid, non_na_date, data)
  except EOFError:
    pass # reached end of file
  logger.info("from %s read %d series", infile, series.count)
  return series
  
def dformat_to_nptype(config, dformat):
  if dformat == DFormat.RAW:
    return np.float64
  if config.int_len == 64:
    return np.int64
  return np.int32

# |FileHeader|SerieMetadata|data as bytes|SerieMetadata|data as bytes| ...
def load_from_np_series(config, infile):
  with common.my_open(config, infile, 'rb') as fileobj:
    header = FileHeader.unpack_from_file(fileobj)
    series = Series(header.dformat)
    dtype = dformat_to_nptype(config, header.dformat)

    for i in range(header.count):
      meta = SerieMetadata.unpack_from_file(fileobj)
      data = np.fromfile(fileobj, dtype, meta.count)
      data = np.ma.masked_invalid(data, copy=False)
      series.add(meta, data)
  return series

# |FileHeader|CompMetadata|...|CompMetadata|data as bytes|data as bytes| ...
def load_from_compressed_series(config, infile):
  return

def dump_as_np_series(config, outfile, series):
  header = FileHeader(FFormat.UNKNOWN, series.dformat, series.count)
  with common.my_open(config, outfile, 'wb') as fileobj:
    fileobj.write(header.pack())
    for meta,data in zip(series.meta, series.data):
      fileobj.write(meta.pack())
      fileobj.write(data.data.tobytes())
      #np.save(fileobj, data.data, allow_pickle=False)

def dump_prob_distribution(config, infile):
  return

