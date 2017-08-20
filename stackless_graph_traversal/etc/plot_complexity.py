import sys, math, os
import pandas as pd
import matplotlib.pyplot as plt

algo_to_col = {
    'DAG'    : 0,
    'DCYCLE' : 1,
    'UCYCLE' : 2,
}

def main ():
  graph_files = sys.argv[1:]
  figure, ax_matrix = prepare_figure(graph_files)
  for row, graph_file in enumerate(graph_files):  
    draw_row_of_graphs(ax_matrix, row, graph_file)
  figure.savefig('complexity_analysis.png', bbox_inches='tight')

def prepare_figure (graph_files):
  figsize = (20, len(graph_files) * 6)
  figure, ax_matrix = plt.subplots(figsize=figsize, ncols=3, nrows=len(graph_files))

  for name, col in algo_to_col.items():
      ax_matrix[0][col].set_title('Graph type : ' + name)
  for row, graph_file in enumerate(graph_files):  
      ax_matrix[row][0].set_ylabel(os.path.basename(graph_file))
  return figure, ax_matrix

def draw_row_of_graphs (ax_matrix, row, graph_file):
  df = pd.read_csv(graph_file, engine='python', sep='\s*,\s*')
  df = df.loc[:, ['name', 'total_time']]
  dfg = df.groupby('name')

  for name, group in dfg:
    algo = name.split('_')[-1]
    col = algo_to_col[algo]
    df_group = shape_up_df_group(group)
    print("\n%s : %s\n" % (graph_file, algo), df_group)
    df_group.plot(ax=ax_matrix[row][col], y='total_time', label=name)

def linear_model (start, slope):
  def __inner_model__(x):
    return start + slope * x.name
  return __inner_model__

def nlogn_model (start, log_slope):
  def __inner_model__(x):
    return start + log_slope * x.name * math.log(x.name or 0.0001)
  return __inner_model__

def shape_up_df_group (df):
  df_group = df.reset_index(drop=True)
  x_len = df_group.shape[0] - 1
  start = df_group.iloc[0].total_time
  end = df_group.iloc[-1].total_time
  slope = (end - start) / x_len
  log_slope = (end - start) / (x_len * math.log(x_len))

  df_group['lin_model'] = df_group.apply(axis=1, func=linear_model(start, slope))
  df_group['log_model'] = df_group.apply(axis=1, func=nlogn_model(start, log_slope))
  return df_group

if __name__ == '__main__':
  main()

