import numpy as np
import pickle, os, gzip, datetime as dt
import unittest as ut
import series_io, series_transform, series_stats_calc
from common import *
logger = logging.getLogger(__name__)

CHIST_COL = '$CHIST'
TDATA_SAMPLES = [17, 7, 19]
CHIST_RAW_FILES = ['sample_bics_1_tech.gz', 'sample_xchng_us.gz']

#@ut.skip('')
class TestSeriesIO (ut.TestCase):

  @classmethod
  def setUpClass(klass):
    klass.config = parse_args('help msg todo')
    #klass.make_test_data(['secout_bics_1_tech.gz', 'secout_xchng_us.gz'], CHIST_RAW_FILES)

  @classmethod
  def make_test_data(klass, src, dst):
    for infile,outfile in zip(src,dst):
      with gzip.open(outfile, 'wb') as outobj:
        with gzip.open(infile, 'rb') as fileobj:
          for i in TDATA_SAMPLES:
            df_multi = pickle.load(fileobj)
            sample = df_multi.index.levels[0].values[:]
            sample = [ s for s in sample if df_multi.loc[s][CHIST_COL].last_valid_index() != None ]
            np.random.shuffle(sample)
            sample = sorted(sample[:i])
            df_multi = df_multi[df_multi.index.get_level_values(0).isin(sample)]
            df_multi.reset_index(inplace=True)
            df_multi.set_index(keys=['BRAINP', 'DATE'], drop=True, inplace=True)
            df_multi.to_pickle(outobj, compression=None)
    return dst

  #@ut.skip('')
  def test_load_from_df_chunks_basic(self):
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    self.assertEqual(len(series.meta), sum( s for s in TDATA_SAMPLES ))
    for meta,data in zip(series.meta, series.data):
      assert len(data) == meta.count

  #@ut.skip('')
  def test_load_from_np_series_idempotent(self):
    first_read = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    dump_path = series_io.dump_as_np_series(
      TestSeriesIO.config, tmp_stage_name(TestSeriesIO.config), first_read)
    second_read = series_io.load_from_np_series(TestSeriesIO.config, dump_path)

    for data1,data2 in zip(first_read.data,second_read.data):
      assert np.ma.allequal(data1, data2), "%r != %r" % (data1, data2)
    for meta1,meta2 in zip(first_read.meta,second_read.meta):
      assert meta1.sid == meta2.sid
      assert meta1.min == meta2.min
      assert meta1.max == meta2.max
      assert meta1.start == meta2.start
      assert meta1.count == meta2.count

  #@ut.skip('')
  def test_dump_as_plain_txt(self):
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    dump_path = series_io.dump_as_plain_txt(
      TestSeriesIO.config, tmp_stage_name(TestSeriesIO.config), series)
    assert os.path.isfile(dump_path)
    with my_open(TestSeriesIO.config, dump_path, 'r') as fileobj:
      lines = fileobj.readlines()
    expected_lines = sum( len(d) + 1 for d in series.data )
    self.assertGreaterEqual(len(lines), expected_lines)

    series = series_transform.normalize_series(self.config, series)
    dump_path = series_io.dump_as_plain_txt(
      TestSeriesIO.config, tmp_stage_name(TestSeriesIO.config), series)
    series = series_transform.normal_to_delta_series(self.config, series)
    dump_path = series_io.dump_as_plain_txt(
      TestSeriesIO.config, tmp_stage_name(TestSeriesIO.config), series)

  #@ut.skip('')
  def test_dump_prob_distribution(self):
    series = series_io.build_gaussian_series(self.config, 0, 128, 10, 2000)
    stats = series_stats_calc.calc_stats_from_delta_series(self.config, series)
    dump_path = series_io.dump_prob_distribution(
      self.config, tmp_stage_name(self.config), stats)
    with open(dump_path, 'r') as fileobj:
      lines = fileobj.readlines()
    self.assertTrue(len(lines) >= len(stats.prob_dstrb))


### END TestSeriesIO

#@ut.skip('')
class TestSeriesTransform (ut.TestCase):

  @classmethod
  def setUpClass(klass):
    klass.config = parse_args('help msg todo')

  def setUp(self):
    self.config = TestSeriesTransform.config

  #@ut.skip('')
  def test_normalize_series(self):
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    norm_series = series_transform.normalize_series(self.config, series)
    lerp_max = series_transform.get_lerp_max(self.config)

    for meta1,meta2 in zip(series.meta, norm_series.meta):
      self.assertEqual(meta1.sid, meta2.sid)
      self.assertEqual(meta1.count, meta2.count)
      self.assertEqual(meta1.start, meta2.start)
    for meta2,data2 in zip(norm_series.meta, norm_series.data):
      vmin, vmax = data2.min(), data2.max()
      self.assertTrue(np.any(data2 == 0))
      self.assertTrue(meta2.max - meta2.min >= MIN_SCALE)
      self.assertTrue(vmax - vmin >= 0, "%r < %r" % (vmin,vmax))
      self.assertTrue(np.all(data2 <= lerp_max), '%r = [%d, %d]' % (meta2, vmin, vmax))

  #@ut.skip('')
  def test_cycle_transformations(self):
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    norm_series = series_transform.normalize_series(self.config, series)
    delta_series = series_transform.normal_to_delta_series(self.config, norm_series)
    raw_series = series_transform.delta_to_raw_series(self.config, delta_series)
    atol = MIN_SCALE / 2 ** (self.config.int_len-1)

    #logger.debug('start : %r\nend : %r', series.data[2], raw_series.data[2])
    for meta1,meta2 in zip(series.meta, raw_series.meta):
      self.assertEqual(meta1.sid, meta2.sid)
      self.assertEqual(meta1.count, meta2.count)
      self.assertEqual(meta1.start, meta2.start)
    for data1,data2 in zip(series.data, raw_series.data):
      outliers = ((data1 - data2) / data2).compressed()
      outliers = outliers[np.logical_or(outliers<-1e-3, outliers>1e-3)]
      self.assertTrue(np.array_equal(data1.mask, data2.mask))
      self.assertTrue(np.allclose(data1, data2, atol=atol, rtol=1e-3), '\n%r' % outliers)

### END TestSeriesTransform

#@ut.skip('')
class TestStatCalculator (ut.TestCase):

  @classmethod
  def setUpClass(klass):
    klass.config = parse_args('help msg todo')

  def setUp(self):
    self.config = TestStatCalculator.config

  def percentiles_ok(self, perc):
    last_v = None
    for k,v in sorted(perc.items()):
      if last_v != None and last_v > v:
        return False
      last_v = v
    return True

  #@ut.skip('')
  def test_stat_calc_on_constant(self):
    series = series_io.build_gaussian_series(self.config, 0, 0, 10, 2000)
    stats = series_stats_calc.calc_stats_from_delta_series(self.config, series)
    self.assertTrue(np.isclose(stats.full_histo.std, 0, rtol=0.05))
    self.assertEqual(stats.full_histo.avg, 0.0)
    self.assertEqual(stats.norm_histo.avg, 0.0)
    self.assertTrue(self.percentiles_ok(stats.full_histo.perc))
    self.assertTrue(self.percentiles_ok(stats.norm_histo.perc))

    series = series_io.build_gaussian_series(self.config, 666, 0, 10, 2000)
    stats = series_stats_calc.calc_stats_from_delta_series(self.config, series)
    self.assertTrue(np.isclose(stats.full_histo.std, 0.0, rtol=0.05))
    self.assertEqual(stats.full_histo.avg, 666)

    mu = self.config.alphabet_len - 2
    series = series_io.build_gaussian_series(self.config, mu, 0, 10, 2000)
    stats = series_stats_calc.calc_stats_from_delta_series(self.config, series)
    logger.info("result=\n%r", stats)
    self.assertTrue(np.isclose(stats.full_histo.std, 0.0, rtol=0.05))
    self.assertTrue(np.isclose(stats.norm_histo.std, 0.0, rtol=0.05))
    self.assertEqual(stats.full_histo.avg, mu)
    self.assertEqual(stats.norm_histo.avg, mu)

  #@ut.skip('')
  def test_aggregate_head_tail(self):
    a = self.config.alphabet_len ** MAX_ALPHA_EXP
    calculator = series_stats_calc.SeriesStats(self.config)

    dstrb = [(-3*a, 1), (-2*a, 2), (-a, 3), (0, 4), (a, 5), (2*a, 6), (3*a, 7), ]
    expected = [(-a, 6), (0, 7), (a, 13), ]
    new_dstrb = calculator.aggregate_head_tail(self.config, dstrb)
    self.assertTrue( all(i==j for i,j in zip(new_dstrb, expected)), 
      "new(%r) != expect(%r)" % (new_dstrb, expected) )

    dstrb = [(-a, 1), (-2, 3), (-1, 4), (0, 7), (1, 8), (2, 9), (a, 11), ]
    expected = [(-a, 1), (-2, 3), (-1, 4), (0, 7), (1, 8), (2, 9), (a, 11), ]
    new_dstrb = calculator.aggregate_head_tail(self.config, dstrb)
    self.assertTrue( all(i==j for i,j in zip(new_dstrb, expected)), 
      "new(%r) != expect(%r)" % (new_dstrb, expected) )

  #@ut.skip('')
  def test_stat_calc_on_random(self):
    series = series_io.build_gaussian_series(self.config, 0, 128, 10, 2000)
    stats = series_stats_calc.calc_stats_from_delta_series(self.config, series)
    #logger.info("result=\n%r", stats)
    self.assertTrue(np.isclose(stats.full_histo.std, 128, rtol=0.05))
    self.assertTrue(np.isclose(stats.full_histo.avg, 0, atol=2))
    self.assertTrue(np.isclose(stats.norm_histo.avg, 0, atol=2))
    self.assertTrue(self.percentiles_ok(stats.full_histo.perc))
    self.assertTrue(self.percentiles_ok(stats.norm_histo.perc))

  #@ut.skip('')
  def test_stat_calc_on_sample(self):
    max_prob = 2 ** self.config.int_len - 1
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    series = series_transform.normalize_series(self.config, series)
    series = series_transform.normal_to_delta_series(self.config, series)
    stats = series_stats_calc.calc_stats_from_delta_series(self.config, series)
    #logger.info("result=\n%r", stats.prob_dstrb)
    self.assertTrue(stats.prob_dstrb[0][1] < max_prob)
    self.assertEqual(stats.prob_dstrb[0][2], stats.prob_dstrb[0][1])
    self.assertEqual(stats.prob_dstrb[-1][1], max_prob)
    self.assertTrue(all( cum > 0 and cum <= max_prob for sym,cum,dlt in stats.prob_dstrb ))
    self.assertTrue(all( dlt > 0 and dlt <= max_prob and dlt <= cum for sym,cum,dlt in stats.prob_dstrb ))
    self.assertTrue(self.percentiles_ok(stats.full_histo.perc))
    self.assertTrue(self.percentiles_ok(stats.norm_histo.perc))

  #@ut.skip('')
  def test_decompose_in_base(self):
    calculator = series_stats_calc.SeriesStats(self.config)
    poly = calculator.decompose_in_base(64, 0)
    self.assertTrue( all(i==j for i,j in zip(poly, [(0, 1)])) )
    poly = calculator.decompose_in_base(64, 12)
    self.assertTrue( all(i==j for i,j in zip(poly, [(12, 1)])) )
    poly = calculator.decompose_in_base(64, 65)
    self.assertTrue( all(i==j for i,j in zip(poly, [(1, 1), (64, 1)])) )
    poly = calculator.decompose_in_base(64, 64**3+64*8+3)
    self.assertTrue( all(i==j for i,j in zip(poly, [(3, 1), (64*8, 1), (64**3, 1)])), repr(poly) )
    poly = calculator.decompose_in_base(64, -12)
    self.assertTrue( all(i==j for i,j in zip(poly, [(-12, 1)])) )
    poly = calculator.decompose_in_base(64, -65)
    self.assertTrue( all(i==j for i,j in zip(poly, [(-1, 1), (-64, 1)])) )
    poly = calculator.decompose_in_base(64, -64**3-64*8-3)
    self.assertTrue( all(i==j for i,j in zip(poly, [(-3, 1), (-64*8, 1), (-64**3, 1)])) )

  #@ut.skip('')
  def test_decompose_in_base_pos(self):
    calculator = series_stats_calc.SeriesStats(self.config)
    poly = calculator.decompose_in_base_pos(64, 12)
    self.assertTrue( all(i==j for i,j in zip(poly, [(12, 1)])) )
    poly = calculator.decompose_in_base_pos(64, 65)
    self.assertTrue( all(i==j for i,j in zip(poly, [(1, 1), (64, 1)])) )
    poly = calculator.decompose_in_base_pos(64, 64**3+64*8+3)
    self.assertTrue( all(i==j for i,j in zip(poly, [(3, 1), (64, 8), (64**2,0), (64**3, 1)])) )
    poly = calculator.decompose_in_base_pos(64, -12)
    self.assertTrue( all(i==j for i,j in zip(poly, [(52, 1), (-64, 1)])), poly )
    poly = calculator.decompose_in_base_pos(64, -65)
    self.assertTrue( all(i==j for i,j in zip(poly, [(63, 1), (-64, 2)])), poly)
    poly = calculator.decompose_in_base_pos(64, -64**3-64*8-3)
    self.assertTrue( all(i==j for i,j in zip(poly, [(61, 1), (64, 55), (64**2,63), (-64**3, 2)])), poly )
    poly = calculator.decompose_in_base_pos(64, -63*64**3-64*8-3)
    self.assertTrue( all(i==j for i,j in zip(poly, [(61, 1), (64, 55), (4096, 63), (262144, 0), (-16777216, 1)])), poly )

### END TestStatCalculator

if __name__ == '__main__':
  ut.main(argv=[__name__])

