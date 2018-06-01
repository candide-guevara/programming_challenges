import numpy as np, math
from common import *
logger = logging.getLogger(__name__)

class Histogram:

  def __init__(self):
    self.buckets = {}
    self.avg, self.std = None, None
    self.perc = {}
    self.count_w = 0
    self.count = 0

  def add_to_buckets(self, val, weight=1):
    self.buckets[val] = self.buckets.get(val, 0) + weight
    self.count_w += weight

  def entropy(self):
    alph_entropy = sum( (w/self.count_w) * math.log2(self.count_w/w) for w in self.buckets.values() )
    sym_per_num = self.count_w / self.count
    assert sym_per_num >= 1
    return sym_per_num * alph_entropy

  def calc_stats(self, config):
    logger.info('calculating stats for %d buckets, ratio %f/%f', len(self.buckets), len(self.buckets), self.count_w)
    count_w, self.avg, self.std = 0,0.0,0.0
    dtype = intlen_to_nptype(config)
    pairs = dict_to_np_pairs(self.buckets, dtype, np.uint64)
    pairs.sort(order='key')

    for val,w in pairs:
      count_w += w
      delta = val - self.avg
      self.avg += w * delta / count_w
      delta2 = val - self.avg
      self.std += delta * delta2 * w
      for perc in range(10,100,10):
        if 100.*count_w/self.count_w >= perc:
          self.perc.setdefault(perc, val)

    logger.debug("perc=%r", { p:self.buckets[v] for p,v in self.perc.items() })
    assert count_w and count_w == self.count_w and self.std >= 0
    self.std = math.sqrt(self.std / self.count_w)

  def __repr__(self):
    return "[avg=%r, std=%r, perc=%r, count=%r, w/count=%r]" \
      % (self.avg, self.std, self.perc, self.count, self.count_w/self.count)

### END Histogram

class SeriesStats:

  def __init__(self, config):
    self.alphabet_len = config.alphabet_len
    self.entropy = None
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
    if num == 0: return [(0,1)]
    coef = 1
    poly = []
    if num < 0:
      coef = -1
      num = -num
    while num:
      q = num % base
      if q: poly.append((coef * q, 1))
      num //= base
      coef *= base
    return poly

  def add_to_buckets(self, val, weight):
    self.full_histo.add_to_buckets(val, weight)
    poly = self.decompose_in_base(self.alphabet_len, val)
    for v,w in poly:
      if w: self.norm_histo.add_to_buckets(v, weight * w)

  def added_points(self, count):
    self.full_histo.count += count
    self.norm_histo.count += count

  def calc_stats(self, config, count_series):
    self.full_histo.calc_stats(config)
    self.norm_histo.calc_stats(config)
    self.entropy = (self.full_histo.entropy(), self.norm_histo.entropy())
    self.prob_dstrb = self.calc_prob_dstrb(config, self.norm_histo.buckets, count_series)

  def calc_prob_dstrb(self, config, buckets, count_series):
    cumsum = 0
    irreg_dstrb = []
    for val,w in sorted(buckets.items()):
      cumsum += w
      irreg_dstrb.append((val, cumsum))
    irreg_dstrb = self.aggregate_head_tail(config, irreg_dstrb)
    cumsum = count_series + irreg_dstrb[-1][1]
    irreg_dstrb.append((END_MARK, cumsum))

    max_prob = 2 ** config.int_len - 1
    scale = max_prob / cumsum
    #logger.debug('scale=%r, max_ratio=%r', scale, max( t[2] for t in irreg_dstrb )/cumsum)
    prob_dstrb = [ (v, int(scale*c)) for v,c in irreg_dstrb ]
    prob_dstrb[-1] = (prob_dstrb[-1][0], max_prob)

    last_cum = 0
    for i in range(len(prob_dstrb)):
      tup = prob_dstrb[i]
      prob_dstrb[i] = (tup[0], tup[1], tup[1] - last_cum)
      last_cum = tup[1]
    return prob_dstrb

  def aggregate_head_tail(self, config, dstrb):
    last_cum, extra_w_min, extra_w_max = 0,0,0
    cutoff_val = self.alphabet_len ** MAX_ALPHA_EXP
    new_dstrb= {}

    for val,cum in dstrb:
      w = cum - last_cum
      last_cum = cum
      if val <= -cutoff_val:
        extra_w_min += w * (val//-cutoff_val - 1)
      elif val > cutoff_val:
        extra_w_max += w * (val//cutoff_val)

    assert extra_w_min >= 0 and extra_w_max >= 0
    for val,cum in dstrb:
      if val <= cutoff_val and val >= -cutoff_val:
        new_dstrb[val] = (val, cum + extra_w_min)
    if cutoff_val in new_dstrb:
      new_dstrb[cutoff_val] = (cutoff_val, new_dstrb[cutoff_val][1] + extra_w_max)
    return [ v for k,v in sorted(new_dstrb.items()) ]

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
    for val,weight in zip(*counts): 
      stats.add_to_buckets(val, weight)
    stats.added_points(len(data) - 1)

  stats.calc_stats(config, len(series.data))
  return stats

