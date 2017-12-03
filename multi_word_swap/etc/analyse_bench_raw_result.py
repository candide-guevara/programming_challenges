import pandas as pd
import json, re, os, sys, argparse, math
import matplotlib
matplotlib.use('Agg', warn=False)
import matplotlib.pyplot as plt

def main():
  config = parse_config(sys.argv)
  set_display_options(config)
  if config.explore_file:
    df, indexes = load_from_json_or_pickle(config, config.explore_file)
    perform_all_analisys(df, indexes)
  if config.scale_file:
    df, indexes = load_from_json_or_pickle(config, config.scale_file)
    draw_scalability_graphs(config, df, indexes)

def perform_all_analisys(df, indexes):
  assert all(indexes[c].any() for c in indexes), "Some indexes are wrong or input data is wrong"
  analysis_to_perform = [
    analysis_single_col_on_count('fancy_align', 1),
    analysis_single_col_on_count('rel_lock', 'acq_rel'),
    analysis_memory_order,
    analysis_join_extra_time,
    analysis_count_vs_posix,
    analysis_thread_variance,
    analysis_wait_vs_real_time,
  ]
  for analysis in analysis_to_perform:
    print('\n\n#### %s ####' % analysis.__name__)
    analysis(df, indexes)

def analysis_single_col_on_count(colname, filter_val):
  def dynamic_analysis(df, indexes):
    df_main_only = df[indexes.main & indexes.cnt]
    df_reduced = df_main_only[['totl_secs', 'real_secs']]
    filter_on = df_main_only[colname] == filter_val
    df_delta = df_reduced[filter_on].reset_index(drop=True) - df_reduced[~filter_on].reset_index(drop=True)

    gain_totl = df_delta.totl_secs.sum() * 200. / df_reduced.totl_secs.sum()
    gain_real = df_delta.real_secs.sum() * 200. / df_reduced.real_secs.sum()

    print("%s gain_totl = %.2f%%\n%s gain_real = %.2f%%" 
          % (colname, gain_totl, colname, gain_real))
    return df_delta
  return dynamic_analysis

def analysis_memory_order(df, indexes):
  df_main_only = df[indexes.main & indexes.cnt]
  df_reduced = df_main_only[['totl_secs', 'real_secs']]
  strict_on  = (df_main_only.acq_lock == 'acq_rel') & (df_main_only.rel_lock == 'acq_rel')
  strict_off = (df_main_only.acq_lock == 'acquire') & (df_main_only.rel_lock == 'release')

  df_delta = df_reduced[strict_off].reset_index(drop=True) - df_reduced[strict_on].reset_index(drop=True)
  gain_totl = df_delta.totl_secs.sum() * 200. / df_reduced.totl_secs.sum()
  gain_real = df_delta.real_secs.sum() * 200. / df_reduced.real_secs.sum()
  print("acq_rel gain_totl = %.2f%%\nacq_rel gain_real = %.2f%%" 
        % (gain_totl, gain_real))
  return df_delta

def analysis_join_extra_time(df, indexes):
  df_thrd_no_wait = df[indexes.thrd & indexes.wait_opt]
  df_main_no_wait = df[indexes.main & indexes.wait_opt]
  df_group_thrd = df_thrd_no_wait.groupby(['function', 'all_thr_count', 'fancy_align', 'acq_lock', 'rel_lock'])
  df_max_thrd = df_thrd_no_wait.loc[df_group_thrd.real_secs.idxmax()]

  #group_key = ['function', 'all_thr_count']
  group_key = ['all_thr_count']
  df_sum_main = df_main_no_wait.groupby(group_key).sum()
  df_sum_thrd = df_max_thrd.groupby(group_key).sum()
  df_avg_main = df_main_no_wait.groupby(group_key).mean()
  df_avg_thrd = df_max_thrd.groupby(group_key).mean()

  df_result = pd.concat([ (df_sum_main - df_sum_thrd).real_secs, df_avg_main.real_secs, df_avg_thrd.real_secs ], axis=1)
  df_result.columns = [ 'delt_wait', 'main_avg', 'thrd_avg' ]

  wait_join = df_result.delt_wait.sum() * 100 / df_sum_main.real_secs.sum()
  print("%r\nwait_join = %.2f%%" % (df_result, wait_join))
  return df_result

def analysis_count_vs_posix(df, indexes):
  alg_idxs = [indexes.cnt, indexes.mutex, indexes.spin, indexes.rwlock]
  alg_names = ['count', 'mutex', 'spin', 'rwlock']
  alg_df = []
  legend = None

  for i, alg_idx in enumerate(alg_idxs):
    df_thrd_no_wait = df[alg_idx & indexes.thrd & indexes.wait_opt]
    df_group_thrd = df_thrd_no_wait.groupby(['function', 'all_thr_count'])
    df_sum_thrd = df_group_thrd.sum()[['totl_secs', 'real_secs']]
    if i == 0:
      legend = df_sum_thrd.index
    alg_df.append(df_sum_thrd.reset_index(drop=True))

  df_base = alg_df[0].copy()
  for i in range(1, len(alg_idxs)):
    tmp = alg_df[0] - alg_df[i]
    df_base['dlt_totl_' + alg_names[i]] = tmp.totl_secs * 100. / df_base.totl_secs
    df_base['dlt_real_' + alg_names[i]] = tmp.real_secs * 100. / df_base.real_secs

  df_base = df_base.round(2)
  df_base.index = legend
  print(df_base)
  return df_base

def analysis_thread_variance(df, indexes):
  alg_idxs = [indexes.cnt, indexes.mutex, indexes.spin, indexes.rwlock]
  alg_names = ['count', 'mutex', 'spin', 'rwlock']
  alg_df = []
  legend = None

  for i, alg_idx in enumerate(alg_idxs):
    df_thrd_no_wait = df[alg_idx & indexes.thrd & indexes.wait_opt]
    df_group_thrd = df_thrd_no_wait.groupby(['function', 'all_thr_count'])
    df_agg_thrd = pd.concat([
      df_group_thrd.mean()[['totl_secs', 'real_secs']],
      df_group_thrd.std()[['totl_secs', 'real_secs']],
    ], axis=1)
    if i == 0:
      legend = df_agg_thrd.index
    alg_df.append(df_agg_thrd.reset_index(drop=True))

  df_base = pd.concat(alg_df, axis=1)
  df_base = df_base.round(2)
  df_base.index = legend
  df_base.columns = [ c for a in alg_names for l in [a+'_avg', a+'_std'] for c in [l+'_t', l+'_r'] ]
  print(df_base)
  return df_base

def analysis_wait_vs_real_time(df, indexes):
  df_thrd_no_wait = df[indexes.cnt & indexes.thrd]
  df_group_thrd = df_thrd_no_wait.groupby(['function', 'wait_quantum'])
  df_agg_thrd = pd.concat([
    df_group_thrd.mean()[['totl_secs', 'real_secs']],
    df_group_thrd.sum()[['totl_secs', 'real_secs']],
  ], axis=1)
  df_agg_thrd.columns = ['totl_avg', 'real_avg', 'totl_sum', 'real_sum']
  print(df_agg_thrd)
  return df_agg_thrd

##############################################################################################

def draw_scalability_graphs(config, df, indexes):
  func_idxs = [indexes.reader, indexes.writer, indexes.rd_wr ]
  titles =  [ f + ' real scalability' for f in ['read', 'write', 'read-write']]
  titles += [ f + ' totl scalability' for f in ['read', 'write', 'read-write']]
  graph_dfs = []

  for i, func_idx in enumerate(func_idxs):
    df_thrd_func = df[func_idx & indexes.thrd]
    df_group_thrd = df_thrd_func.groupby(['function', 'all_thr_count'])
    df_avg_thrd = df_group_thrd.mean()[['totl_secs', 'real_secs']]

    graph_df = df_avg_thrd.unstack(level=0)
    old_cols = graph_df.columns.levels
    graph_df.columns = [ '%s_%s' % (m.split('_')[0], f.split('_')[0])
                         for m in old_cols[0] for f in old_cols[1] ]
    graph_dfs.append(graph_df.filter(regex='real')) 
    graph_dfs.append(graph_df.filter(regex='totl')) 

  graph_dfs = [ g for i,g in enumerate(graph_dfs) if i%2 == 0 ] \
            + [ g for i,g in enumerate(graph_dfs) if i%2 == 1 ]
  plot_df_list(config, graph_dfs, titles)
  return graph_dfs

##############################################################################################

def load_from_json_or_pickle(config, infile):
  pickle_df = infile + '.pickle'
  pickle_ix = infile + '.idx.pickle'

  if os.path.exists(pickle_df) and config.use_cache:
    df = pd.read_pickle(pickle_df)
  else:
    json_objs = build_json_objects_from_stdin_or_file(infile)
    df = json_objects_to_pandas_df(json_objs)
    df.to_pickle(pickle_df)

  if os.path.exists(pickle_ix) and config.use_cache:
    indexes = pd.read_pickle(pickle_ix)
  else:
    indexes = build_common_indexes(df)
    indexes.to_pickle(pickle_ix)

  return df, indexes
  
def build_common_indexes(df):
  indexes = pd.DataFrame()
  indexes['main'] = df.counter_type == 'main'
  indexes['thrd'] = df.counter_type == 'thrd'
  indexes['wait_opt'] = df.wait_quantum == 127

  indexes['cnt'] = df.function.str.contains('count_')
  indexes['mutex'] = df.function.str.contains('mutex_')
  indexes['spin'] = df.function.str.contains('spin_')
  indexes['rwlock'] = df.function.str.contains('rwlock_')

  indexes['reader'] = df.function.str.contains('_readers')
  indexes['writer'] = df.function.str.contains('_writers')
  indexes['rd_wr']  = df.function.str.contains('_rd_wr')

  indexes['release'] = df.rel_lock.str.contains('release')
  indexes['acquire'] = df.acq_lock.str.contains('acquire')

  #print(indexes)
  return indexes

def build_json_objects_from_stdin_or_file(infile):
  json_objs = []
  with open(infile, 'r') as fileobj:
    for line in fileobj:
      json_objs.append( json.loads(line) )
  #print(json_objs)
  return json_objs
  
def json_objects_to_pandas_df(json_objs):
  cols, params = get_column_labels(json_objs[0])
  rows = []
  for obj in json_objs:
    param_list = build_params_cols(params, obj)
    row = [ 'main', 0, 
      round(obj['main']['user_secs'], 2), 
      round(obj['main']['syst_secs'], 2), 
      round(obj['main']['user_secs'] + obj['main']['syst_secs'], 2),
      round(obj['main']['real_secs'] , 2),
    ]
    rows.append(param_list + row)

    for i,thread_res in enumerate(obj['results']):
      row = [ 'thrd', i, 
        round(thread_res['user_secs'], 2), 
        round(thread_res['syst_secs'], 2), 
        round(thread_res['user_secs'] + thread_res['syst_secs'], 2),
        round(thread_res['real_secs'], 2), 
      ]
      rows.append(param_list + row)

  df = pd.DataFrame(rows, columns=cols)
  #print(df)
  return df

SIMPLIFY_RX = re.compile('bench_|concurrent_|memory_order_')
def build_params_cols(params, obj):
  items = []
  for v in (obj['params'][p] for p in params):
    if isinstance(v, str):
      items.append(SIMPLIFY_RX.sub('', v))
    else: items.append(v)
  return items

def get_column_labels(obj):
  param_filter = ("BENCH_FUNCTION", "BENCH_FANCY_ALIGN", "BENCH_ALL_THR_COUNT", "MEMORY_ORDER_REL_LOCK", "MEMORY_ORDER_ACQ_LOCK", "WAIT_QUANTUM")
  cols = ['counter_type', 'counter_idx', 'user_secs', 'syst_secs', 'totl_secs', 'real_secs']
  params = sorted(k for k in obj['params'].keys() if k in param_filter)
  cols = [ SIMPLIFY_RX.sub('', p.lower()) for p in params ] + cols
  return cols, params

##############################################################################################

def set_display_options(config):
  pd.set_option('display.width', 200)
  pd.set_option('display.max_colwidth', 32)
  pd.set_option('display.max_rows', 20)
  pd.set_option('display.max_columns', 40)

def plot_df_list(config, graph_dfs, titles):
  rows = int(math.sqrt(len(graph_dfs)))
  cols = (rows == 1 and len(graph_dfs)) \
         or (rows**2 == len(graph_dfs) and rows) \
         or rows+1

  plots, axes = plt.subplots(nrows=rows, ncols=cols, sharey=True)
  for graph_df,title,ax in zip(graph_dfs, titles, axes.flat):
    graph_df.plot(ax=ax, title=title)
  plots.set_size_inches(cols*6, rows*3 + 2)
  plots.tight_layout()
  plots.savefig(config.graph_out)

  if config.interactive:
    plt.switch_backend('TkAgg')
    plt.ioff()
    plots, axes = plt.subplots(nrows=rows, ncols=cols, sharey=True)
    for graph_df,title,ax in zip(graph_dfs, titles, axes.flat):
      graph_df.plot(ax=ax, title=title)
    plots.set_size_inches(cols*4 + 1, rows*3 + 1)
    plots.tight_layout()
    plt.show()

def parse_config(cmd_args):
  parser = argparse.ArgumentParser('Multiswap benchmark raw result analysis')
  parser.add_argument ('--explore-file', '-e', 
                        help='Benchmark raw results for exploratory analysis', default='')
  parser.add_argument ('--scale-file', '-s', 
                        help='Benchmark raw results for scalability analysis', default='')
  parser.add_argument ('--graph-out', '-g', 
                        help='The filename to write the plots into', default='analysis_graph.svg')
  parser.add_argument ('--interactive', '-i', 
                        help='Experimental: show plots in a new window', action='store_true')
  parser.add_argument ('--use-cache', '-c', 
                        help='If available use intermediary dataframe caches', action='store_true')
  config = parser.parse_args()
  return config

##############################################################################################

if __name__ == "__main__":
  main()

