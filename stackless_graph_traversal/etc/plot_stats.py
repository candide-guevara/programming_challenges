import sys, math, os, re
import pandas as pd
import matplotlib.pyplot as plt

def main ():
  hw_counter_files = sys.argv[1:]
  figure, ax_matrix = prepare_figure(hw_counter_files)
  for idx,hw_counter_file in enumerate(hw_counter_files):
    draw_hw_counter_file(idx, hw_counter_file, ax_matrix)
  figure.savefig('hw_counters.png', bbox_inches='tight')

def prepare_figure (hw_counter_files):
  nbfiles = len(hw_counter_files)
  figsize = (24, nbfiles * 3)
  nrows = (nbfiles + 1) // 2
  figure, ax_matrix = plt.subplots(figsize=figsize, ncols=2, nrows=nrows)

  for idx,title in enumerate(hw_counter_files):
    short_title = os.path.basename( title.split('.')[0] )
    ax_matrix[idx//2][idx%2].set_title(short_title)
  return figure, ax_matrix

def draw_hw_counter_file (idx, hw_counter_file, ax_matrix):
  df_hw_counters = pd.read_csv(hw_counter_file,
    comment='#', usecols=(2,0), index_col='counter_name', names=('counter_value','counter_name'))

  cycles = df_hw_counters.loc['cycles'].counter_value
  instructions = df_hw_counters.loc['instructions'].counter_value
  df_hw_counters['metric_per_1000c'] = df_hw_counters.counter_value * 1000 / cycles
  df_hw_counters['metric_per_1000i'] = df_hw_counters.counter_value * 1000 / instructions
  df_hw_counters['short_name'] = df_hw_counters.index.to_series().apply(shorten_counter_name)

  ax = ax_matrix[idx//2][idx%2]
  # we omit the first value which is cycles
  df_hw_counters.iloc[1:].plot( 
    ax=ax, 
    kind='barh', 
    x='short_name', 
    y=[ 'metric_per_1000i', 'metric_per_1000c'], 
    logx=True)
  ax.set_ylabel("")
  ax.xaxis.grid(True)
  print (calc_interesting_ratios(hw_counter_file, df_hw_counters))

def calc_interesting_ratios (hw_counter_file, df_hw_counters):
  branches = df_hw_counters.loc['branches'].counter_value
  b_misses = df_hw_counters.loc['branch-misses'].counter_value
  l1_rd_total = df_hw_counters.loc['L1-dcache-loads'].counter_value
  l1_misses = df_hw_counters.loc['L1-dcache-load-misses'].counter_value
  tbl_total = df_hw_counters.loc['dTLB-loads'].counter_value
  tbl_misses = df_hw_counters.loc['dTLB-load-misses'].counter_value

  report = """%s
  - b_misses / branches = %.2e
  - l1_misses / l1_rd_total = %.2e
  - tbl_misses / tbl_total = %.2e
  """ % (
    hw_counter_file,
    b_misses / branches,
    l1_misses / l1_rd_total,
    tbl_misses / tbl_total,
  )
  return report

rx_load = re.compile('loads?')
rx_cycle = re.compile('cycles?')
rx_miss = re.compile('misses')
rx_all = re.compile('.?(all|any)')
def shorten_counter_name(name):
  name = rx_load .sub('ld', name)
  name = rx_cycle.sub('cl', name)
  name = rx_miss .sub('miss', name)
  name = rx_all  .sub('', name)
  return name

if __name__ == '__main__':
  main()

