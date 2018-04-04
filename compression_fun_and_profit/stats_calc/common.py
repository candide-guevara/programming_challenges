import logging, sys, os, re, argparse, gzip, uuid

def my_open(config, filepath, mode):
  if config.use_gzip:
    return gzip.open(filepath, mode)
  return open(filepath, mode)

def tmp_stage_name(config):
  return os.path.join(config.stage_dir, str(uuid.uuid1()))

def parse_args (help_msg):
  parser = argparse.ArgumentParser(help_msg)
  parser.add_argument ('--stage-dir', '-s', 
                        help='Directory for staging temp result files',
                        default='/tmp')
  parser.add_argument ('--raw-input', '-i', 
                        help='Gzipped file containing input dataframes')
  parser.add_argument ('--col-name', '-c', 
                        help='The name of the series to process')
  parser.add_argument ('--prob-input', '-p', 
                        help='File containing the probability distribution')
  parser.add_argument ('--use-gzip',
                        help='Use gzip to store results in staging',
                        action='store_true',
                        default=False)
  parser.add_argument ('--int-len',
                        help='The len of each normalized price in bits',
                        default=32)
  args = parser.parse_args()
  return args

def __init_log__ ():
  if False:
    format = '%(levelname)s-%(name)s::%(funcName)s-> %(message)s'
  else:
    blueFnt = '\033[94m'
    greenFnt = '\033[92m'
    resetFnt = '\033[0m'
    format = '%s%%(levelname)s-%s%%(name)s::%%(funcName)s-> %s%%(message)s' % (blueFnt, greenFnt, resetFnt)
  logging.basicConfig(level = logging.DEBUG, format = format)

__init_log__()

