import pandas as pd, numpy as np, gzip
import common, copy
logger = common.logging.getLogger(__name__)

def normalize_series(config, series):
  assert series.dformat == DFormat.RAW
  new_series = Series(DFormat.NORMAL)
  for meta,data in zip(series.meta, series.data):
    new_meta = copy.copy(meta)
    new_meta.min = data.min()
    new_meta.max = data.max()
    new_series.add(new_meta, new_data)
  logger.info("to normal %d series", series.count)
  return new_series

def normal_to_delta_series(config, series):
  assert series.dformat == DFormat.NORMAL
  new_series = Series(DFormat.DELTA)
  for meta,data in zip(series.meta, series.data):
    new_meta = copy.copy(meta)
    new_series.add(new_meta, new_data)
  logger.info("to delta %d series", series.count)
  return new_series

def delta_to_raw_series(config, series):
  assert series.dformat == DFormat.DELTA
  new_series = Series(DFormat.RAW)
  for meta,data in zip(series.meta, series.data):
    new_meta = copy.copy(meta)
    new_series.add(new_meta, new_data)
  logger.info("to raw prices %d series", series.count)
  return new_series

