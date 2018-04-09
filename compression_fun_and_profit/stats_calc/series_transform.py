import pandas as pd, numpy as np, gzip
import common, copy, math
from series_io import *
logger = common.logging.getLogger(__name__)

def is_series_abnormal(config, meta):
  assert meta.min > 0
  if meta.max/meta.min > config.min_max_ratio:
    logger.info('rejecting %r because max/min is too wide', meta)
    return True
  if math.isclose(meta.min, meta.max, rel_tol=config.min_max_equal):
    logger.info('rejecting %r because min == max', meta)
    return True
  return False

def get_lerp_max(config):
  return 2**(config.int_len - 1) - 1

def normalize_series(config, series):
  assert series.dformat == DFormat.RAW
  new_series = Series(DFormat.NORMAL)
  lerp_max = get_lerp_max(config)
  int_type = dformat_to_nptype(config, DFormat.NORMAL)

  for meta,data in zip(series.meta, series.data):
    assert not data.mask[0], 'serie should start with non Na : %r\n%r' % (meta,data)
    new_meta = copy.copy(meta)
    new_meta.min = data.min()
    new_meta.max = data.max()
    if is_series_abnormal(config, new_meta):
      continue
    new_data = lerp_max * ((data - new_meta.min) / (new_meta.max - new_meta.min))
    new_data = new_data.astype(int_type)
    new_series.add(new_meta, new_data)
  logger.info("to normal %d series", series.count)
  return new_series

def normal_to_delta_series(config, series):
  assert series.dformat == DFormat.NORMAL
  new_series = Series(DFormat.DELTA)
  for meta,data in zip(series.meta, series.data):
    new_meta = copy.copy(meta)
    new_data = np.ediff1d(data.filled(0), to_begin=data[:1])
    new_data = np.ma.MaskedArray(new_data, data.mask)
    new_series.add(new_meta, new_data)
  logger.info("to delta %d series", series.count)
  return new_series

def delta_to_raw_series(config, series):
  assert series.dformat == DFormat.DELTA
  new_series = Series(DFormat.RAW)
  for meta,data in zip(series.meta, series.data):
    new_meta = copy.copy(meta)
    new_meta.min = None
    new_meta.max = None
    new_data = np.cumsum(data.base)
    new_data = meta.min + ((meta.max-meta.min) * new_data) / get_lerp_max(config)
    new_data = np.ma.MaskedArray(new_data, data.mask)
    new_series.add(new_meta, new_data)
  logger.info("to raw prices %d series", series.count)
  return new_series

