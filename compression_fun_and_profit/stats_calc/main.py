import common, series_io

if __name__ == '__main__':
  config = common.parse_args('help msg todo')
  s = series_io.load_from_df_chunks(config.raw_input, config.col_name)
  meta = s.meta[0]
  print(meta.sid,meta.min,meta.max,meta.start,meta.count)
  series_io.dump_as_np_series(config, config.stage_dir + '/np_dump.gz', s)
  s2 = series_io.load_from_np_series(config, config.stage_dir + '/np_dump.gz')
  meta = s2.meta[0]
  print(meta.sid,meta.min,meta.max,meta.start,meta.count)
  #series_io.dump_as_np_series(config, config.stage_dir + '/np_dump2.gz', s)

