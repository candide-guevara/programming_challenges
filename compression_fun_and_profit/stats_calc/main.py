import series_io, series_transform, series_stats_calc
from common import *
logger = logging.getLogger(__name__)

def main(config):
  logger.info('start stat calculation')
  series = series_io.load_from_df_chunks(config.raw_input, config.col_name)
  series = series_transform.normalize_series(config, series)
  series = series_transform.normal_to_delta_series(config, series)
  stats = series_stats_calc.calc_stats_from_delta_series(config, series)
  logger.info('stats=%r', stats)
  series_io.dump_prob_distribution(config, config.prob_output, stats.prob_dstrb)
  logger.info('prob dstrb dumped into %s', config.prob_output)

if __name__ == '__main__':
  config = parse_args('help msg todo')
  main(config)

