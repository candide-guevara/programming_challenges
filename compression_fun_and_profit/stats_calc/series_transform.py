import numpy as np, gzip
import copy, math
from series_io import *
from common import *
logger = logging.getLogger(__name__)

def is_series_abnormal(config, meta):
  assert meta.min >= 0
  if meta.max/(1+meta.min) > MAX_PRICE_RATIO:
    logger.info('rejecting %r because max/min is too wide', meta)
    return True
  if meta.max > MAX_ABS_PRICE:
    logger.info('rejecting %r because max price is too high', meta)
    return True
  return False

# if (vmin-vmax) is too small, it is overkill to scale to the whole available range
def choose_suitable_range(vmin, vmax):
  vrange = (vmin, vmax)
  if (vmax - vmin) < MIN_SCALE:
    vrange = (vmin, vmin+MIN_SCALE)
  assert vrange[0] < vrange[1]
  return vrange

def highlight_wtf_deltas(config, meta, deltas):
  #assert any(deltas.compressed() < 0)
  index = np.absolute(deltas) > 2 ** (config.int_len - 4)
  wtf = deltas[index].compressed()
  if len(wtf):
    logger.warn("Got huge deltas %r -> %r", meta, wtf)

def normalize_series(config, series):
  assert series.dformat == DFormat.RAW
  new_series = Series(DFormat.NORMAL)
  lerp_max = get_lerp_max(config)
  int_type = dformat_to_nptype(config, DFormat.NORMAL)

  for meta,data in zip(series.meta, series.data):
    assert not data.mask[0], 'serie should start with non Na : %r\n%r' % (meta,data)
    new_meta = copy.copy(meta)
    new_meta.min, new_meta.max = choose_suitable_range(data.min(), data.max())
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
    new_data = np.ediff1d(data.compressed(), to_begin=data[:1])
    nan_index = np.flatnonzero(data.mask)
    nan_index -= np.arange(len(nan_index))
    new_data = np.insert(new_data, nan_index, 0)
    new_data = np.ma.MaskedArray(new_data, data.mask)
    new_series.add(new_meta, new_data)
    #highlight_wtf_deltas(config, meta, new_data)
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

