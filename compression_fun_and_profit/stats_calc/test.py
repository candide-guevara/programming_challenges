import pandas as pd, numpy as np, gzip
import pickle
import common, series_io, series_transform
import unittest as ut
logger = common.logging.getLogger(__name__)

CHIST_COL = '$CHIST'
TDATA_SAMPLES = [17, 7, 19]
CHIST_RAW_FILES = ['test_chist_phmsc_bics_1_tech.gz', 'test_chist_phmsc_xchng_us.gz']

@ut.skip('')
class TestSeriesIO (ut.TestCase):

  @classmethod
  def setUpClass(klass):
    klass.config = common.parse_args('help msg todo')
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

  def test_load_from_df_chunks_basic(self):
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    self.assertEqual(len(series.meta), sum( s for s in TDATA_SAMPLES ))
    for meta,data in zip(series.meta, series.data):
      assert len(data) == meta.count

  def test_load_from_np_series_idempotent(self):
    first_read = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    dump_path = series_io.dump_as_np_series(
      TestSeriesIO.config, common.tmp_stage_name(TestSeriesIO.config), first_read)
    second_read = series_io.load_from_np_series(TestSeriesIO.config, dump_path)

    for data1,data2 in zip(first_read.data,second_read.data):
      assert np.ma.allequal(data1, data2), "%r != %r" % (data1, data2)
    for meta1,meta2 in zip(first_read.meta,second_read.meta):
      assert meta1.sid == meta2.sid
      assert meta1.min == meta2.min
      assert meta1.max == meta2.max
      assert meta1.start == meta2.start
      assert meta1.count == meta2.count

### END TestSeriesIO

class TestSeriesTransform (ut.TestCase):

  @classmethod
  def setUpClass(klass):
    klass.config = common.parse_args('help msg todo')
    #klass.make_test_data(['secout_bics_1_tech.gz', 'secout_xchng_us.gz'], CHIST_RAW_FILES)

  def setUp(self):
    self.config = TestSeriesTransform.config

  def test_normalize_series(self):
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    norm_series = series_transform.normalize_series(self.config, series)
    lerp_max = series_transform.get_lerp_max(self.config)

    for meta1,meta2 in zip(series.meta, norm_series.meta):
      self.assertEqual(meta1.sid, meta2.sid)
      self.assertEqual(meta1.count, meta2.count)
      self.assertEqual(meta1.start, meta2.start)
    for meta2,data2 in zip(norm_series.meta, norm_series.data):
      self.assertTrue(np.any(data2 == 0))
      self.assertTrue(np.any(data2 == lerp_max), '%r = %d/%d' % (meta2, data2.min(), data2.max()))

  def test_cycle_transformations(self):
    series = series_io.load_from_df_chunks(CHIST_RAW_FILES[0], CHIST_COL)
    norm_series = series_transform.normalize_series(self.config, series)
    delta_series = series_transform.normal_to_delta_series(self.config, norm_series)
    raw_series = series_transform.delta_to_raw_series(self.config, delta_series)

    logger.debug('start : %r\nend : %r', series.data[2], raw_series.data[2])
    for meta1,meta2 in zip(series.meta, raw_series.meta):
      self.assertEqual(meta1.sid, meta2.sid)
      self.assertEqual(meta1.count, meta2.count)
      self.assertEqual(meta1.start, meta2.start)
    for data1,data2 in zip(series.data, raw_series.data):
      self.assertTrue(np.allclose(data1, data2, rtol=1e-3), '\n%r\n%r' % (data1, data2))

### END TestSeriesTransform

##############################################################################

if __name__ == '__main__':
  ut.main(argv=[__name__])

