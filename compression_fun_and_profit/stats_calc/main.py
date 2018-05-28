import series_io, series_transform, series_stats_calc
from common import *
logger = logging.getLogger(__name__)

def main(config):
  logger.info('main start')
  dump_test_series(config)
  calculate_statistics(config, 'sample_bics_1_tech')
  calculate_statistics(config, 'sample_xchng_us')
  calculate_statistics(config, 'secout_bics_1_tech')
  calculate_statistics(config, 'secout_xchng_us')

  #dump_real_series(config)
  #dump_as_plain_txt(config)
  logger.info('main end')

def calculate_statistics(config, prefix):
  infile = prefix + '.gz'
  series_out, stats_out = tmp_stage_name(config, prefix + '.bin'), tmp_stage_name(config, prefix + '.prob')
  series = series_io.load_from_df_chunks(infile, config.col_name)
  series = series_transform.normalize_series(config, series)
  series = series_transform.normal_to_delta_series(config, series)
  stats = series_stats_calc.calc_stats_from_delta_series(config, series)
  logger.info('stats=%r', stats)
  series_io.dump_as_np_series(config, series_out, series)
  series_io.dump_prob_distribution(config, stats_out, stats)
  logger.info('%s bin=%r, stats=%r', infile, series_out, stats_out)

def dump_test_series(config):
  series = series_io.build_gaussian_series(config, 0, 1000, 3, 2000)
  stats = series_stats_calc.calc_stats_from_delta_series(config, series)
  series_out, stats_out = tmp_stage_name(config, 'gaussian_series_mu_0.bin'), tmp_stage_name(config, 'gaussian_series_mu_0.prob')
  series_io.dump_as_np_series(config, series_out, series)
  series_io.dump_prob_distribution(config, stats_out, stats)
  logger.info('gaussian series 0=%r, stats=%r', series_out, stats_out)

  series = series_io.build_gaussian_series(config, 666, 10000, 3, 2000)
  stats = series_stats_calc.calc_stats_from_delta_series(config, series)
  series_out, stats_out = tmp_stage_name(config, 'gaussian_series_mu_666.bin'), tmp_stage_name(config, 'gaussian_series_mu_666.prob')
  series_io.dump_as_np_series(config, series_out, series)
  series_io.dump_prob_distribution(config, stats_out, stats)
  logger.info('gaussian series 666=%r, stats=%r', series_out, stats_out)

  series = series_io.build_ordered_series(config, 3, 2000)
  stats = series_stats_calc.calc_stats_from_delta_series(config, series)
  series_out, stats_out = tmp_stage_name(config, 'ordered_series.bin'), tmp_stage_name(config, 'ordered_series.prob')
  series_io.dump_as_np_series(config, series_out, series)
  series_io.dump_prob_distribution(config, stats_out, stats)
  logger.info('ordered series=%r, stats=%r', series_out, stats_out)

def dump_as_plain_txt(config):
  series = series_io.load_from_df_chunks(config.raw_input, config.col_name)
  dump_path = series_io.dump_as_plain_txt(config, tmp_stage_name(config), series)
  logger.info('raw series=%r', dump_path)
  series = series_transform.normalize_series(config, series)
  dump_path = series_io.dump_as_plain_txt(config, tmp_stage_name(config), series)
  logger.info('normal series=%r', dump_path)
  series = series_transform.normal_to_delta_series(config, series)
  dump_path = series_io.dump_as_plain_txt(config, tmp_stage_name(config), series)
  logger.info('delta series=%r', dump_path)

def dump_real_series(config):
  series = series_io.load_from_df_chunks(config.raw_input, config.col_name)
  norm_series = series_transform.normalize_series(config, series)
  delta_series = series_transform.normal_to_delta_series(config, norm_series)
  dump_path = series_io.dump_as_np_series(config, tmp_stage_name(config), delta_series)
  logger.info('%r=%r / %r', config.raw_input, dump_path, delta_series)

if __name__ == '__main__':
  config = parse_args('help msg todo')
  main(config)

