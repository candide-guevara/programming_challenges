import numpy as np, math
from common import *
logger = logging.getLogger(__name__)

class Histogram:

  def __init__(self):
    self.buckets = {}
    self.avg, self.std = None, None
    self.min, self.max = None, None
    self.perc = {}
    self.count = 0

  def add_to_buckets(self, val, weight=1):
    self.buckets[val] = self.buckets.get(val, 0) + weight
    self.count += weight

  def propose_min_max(self, vmin, vmax):
    if self.max == None or self.max < vmax: self.max = vmax
    if self.min == None or self.min > vmin: self.min = vmin

  def entropy(self):
    return sum( (w/self.count) * math.log2(self.count/w) for w in self.buckets.values() )

  def calc_stats(self, config):
    logger.info('calculating stats for %d buckets, ratio %f/%f', len(self.buckets), len(self.buckets), self.count)
    count, self.avg, self.std = 0,0.0,0.0
    dtype = intlen_to_nptype(config)
    pairs = dict_to_np_pairs(self.buckets, dtype, np.uint64)
    pairs.sort(order='key')

    for val,w in pairs:
      count += w
      delta = val - self.avg
      self.avg += w * delta / count
      delta2 = val - self.avg
      self.std += delta * delta2 * w
      for perc in (25, 50, 75):
        if 100*count/self.count >= perc:
          self.perc.setdefault(perc, val)

    #logger.debug("avg=%r, std=%r, count=%r/%r", self.avg, self.std, self.count, count)
    assert count and count == self.count and self.std >= 0
    self.std = math.sqrt(self.std / self.count)

  def __repr__(self):
    return "[avg=%r, std=%r, min=%r, max=%r, perc=%r]" \
      % (self.avg, self.std, self.min, self.max, self.perc)

### END Histogram

class SeriesStats:

  def __init__(self, config):
    self.alphabet_len = config.alphabet_len
    self.entroopy = None
    self.full_histo = Histogram()
    self.norm_histo = Histogram()
    self.prob_dstrb = []

  def decompose_in_base_pos(self, base, num):
    poly = [(num % base, 1)]
    i = 0
    while abs(num) >= base:
      num //= base
      i += 1
      poly.append((base**i, num % base))
    if num < 0:
      if i:
        poly[-1] = (-base**i, -num)
      else:
        poly.append((-base, 1))
    return poly

  def decompose_in_base(self, base, num):
    sign = 1
    if num < 0:
      sign = -1
      num = -num
    poly = [(sign * (num % base), 1)]
    i = 0
    while num >= base:
      num //= base
      i += 1
      poly.append((sign * base**i, num % base))
    return poly

  def add_to_buckets(self, val, weight):
    self.full_histo.add_to_buckets(val, weight)
    poly = self.decompose_in_base(self.alphabet_len, val)
    for v,w in poly:
      if w: self.norm_histo.add_to_buckets(v, weight * w)

  def propose_min_max(self, vmin, vmax):
    self.full_histo.propose_min_max(vmin, vmax)
    poly1 = self.decompose_in_base(self.alphabet_len, vmin)
    poly2 = self.decompose_in_base(self.alphabet_len, vmax)
    self.norm_histo.propose_min_max(poly1[-1][0], poly2[-1][0])

  def calc_stats(self, config, count_series):
    self.full_histo.calc_stats(config)
    self.norm_histo.calc_stats(config)
    self.entropy = self.full_histo.entropy()
    self.prob_dstrb = self.calc_prob_dstrb(self.norm_histo.buckets, count_series, config.int_len)

  def calc_prob_dstrb(self, buckets, count_series, int_len):
    cumsum = count_series
    max_prob = 2 ** int_len
    irreg_dstrb = [(END_MARK, cumsum, count_series)]
    for val,w in sorted(buckets.items()):
      cumsum += w
      irreg_dstrb.append((val, cumsum, w))

    scale = max_prob / cumsum
    #logger.debug('scale=%r, max_ratio=%r', scale, max( t[2] for t in irreg_dstrb )/cumsum)
    prob_dstrb = [ (v, int(scale*c), int(scale*w)) for v,c,w in irreg_dstrb ]
    prob_dstrb[-1] = (prob_dstrb[-1][0], max_prob, prob_dstrb[-1][2])
    return prob_dstrb


  def __repr__(self):
    return "entropy=%r, \nfull=%r, \nnorm=%r, \ndstrb=%r" \
      % (self.entropy, self.full_histo, self.norm_histo, self.prob_dstrb)

### END SeriesStats

def filter_not_relevant_series(data):
  if len(data) < MIN_DATAPOINTS:
    return True
  return False

# the alphabet will be composed of :
# [ ... -alphabet_len**2, -alphabet_len, ... 0, 1, ... alphabet_len, alphabet_len**2 ... END_MARK ]
def calc_stats_from_delta_series(config, series):
  logger.info('calculating stats for %r', series)
  stats = SeriesStats(config)

  for progress,data in enumerate( d.base for d in series.data ):
    if filter_not_relevant_series(data): continue
    if progress % 200 == 0:
      logger.info('progress[%d]: %r', int(100*progress/series.count), series.meta[progress])
    # we do not count the first element because it is not a delta
    counts = np.unique(data[1:], return_counts=True)
    stats.propose_min_max(counts[0][0], counts[0][-1])
    for val,weight in zip(*counts): 
      stats.add_to_buckets(val, weight)

  stats.calc_stats(config, len(series.data))
  return stats

