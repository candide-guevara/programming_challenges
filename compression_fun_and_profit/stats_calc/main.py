import series_io, series_transform, series_stats_calc
from common import *
logger = logging.getLogger(__name__)

def main(config):
  logger.info('main start')
  dump_real_series(config)
  #dump_as_plain_txt(config)
  #dump_test_series(config)
  #calculate_statistics(config)
  logger.info('main end')

def calculate_statistics(config):
  series = series_io.load_from_df_chunks(config.raw_input, config.col_name)
  series = series_transform.normalize_series(config, series)
  series = series_transform.normal_to_delta_series(config, series)
  stats = series_stats_calc.calc_stats_from_delta_series(config, series)
  logger.info('stats=%r', stats)
  series_io.dump_prob_distribution(config, config.prob_output, stats)
  logger.info('prob dstrb dumped into %s', config.prob_output)

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

def dump_test_series(config):
  series = series_io.build_gaussian_series(config, 0, 1000, 3, 2000)
  dump_path = series_io.dump_as_np_series(config, tmp_stage_name(config), series)
  logger.info('gaussian series 0=%r', dump_path)
  series = series_io.build_gaussian_series(config, 666, 10000, 3, 2000)
  dump_path = series_io.dump_as_np_series(config, tmp_stage_name(config), series)
  logger.info('gaussian series 666=%r', dump_path)
  series = series_io.build_ordered_series(config, 3, 2000)
  dump_path = series_io.dump_as_np_series(config, tmp_stage_name(config), series)
  logger.info('ordered series=%r', dump_path)

def dump_real_series(config):
  series = series_io.load_from_df_chunks(config.raw_input, config.col_name)
  norm_series = series_transform.normalize_series(config, series)
  delta_series = series_transform.normal_to_delta_series(config, norm_series)
  dump_path = series_io.dump_as_np_series(config, tmp_stage_name(config), delta_series)
  logger.info('%r=%r / %r', config.raw_input, dump_path, delta_series)

if __name__ == '__main__':
  config = parse_args('help msg todo')
  main(config)

