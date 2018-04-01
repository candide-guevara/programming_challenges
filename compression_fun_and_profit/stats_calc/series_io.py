import pandas as pd, numpy as np
import pickle
logger = logging.getLogger(__name__)

class FileHeader:
  __slots__ = ('fformat', 'dformat', 'count')

class CompMetadata:
  __slots__ = ('id', 'start', 'min', 'max')

class SerieMetadata:
  __slots__ = ('id', 'start', 'min', 'max', 'len')

class Series:
  def __init__(self):
    self.meta = []
    self.data = []
    self.count = 0
    self.dformat = None

# array of dataframes
# each dataframe contains several series (df multiindex on security id)
def load_from_df_chunks(config, infile):
  series = Series()
  try:
    with open(pickle_file, 'rb') as fileobj:
      while True:
        df = pickle.load(fileobj)
  except EOFError:
    pass # reached end of file
  logger.info("from %s read %d series", infile, series.count)
  return series
  
# |FileHeader|SerieMetadata|data as bytes|SerieMetadata|data as bytes| ...
def load_from_np_series(config, infile):
  return

# |FileHeader|CompMetadata|...|CompMetadata|data as bytes|data as bytes| ...
def load_from_compressed_series(config, infile):
  return

def dump_as_np_series(config, infile):
  return

def dump_prob_distribution(config, infile):
  return

