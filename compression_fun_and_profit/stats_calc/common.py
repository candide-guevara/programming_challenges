import numpy as np, gzip
import logging, sys, os, re, argparse, gzip, uuid, datetime

MAX_PRICE_RATIO = 10**6
MAX_ABS_PRICE   = 10**8
MIN_SCALE       = 40_000
MIN_DATAPOINTS  = 50
END_MARK        = None

class DFormat:
  RAW,NORMAL,DELTA,BYTE_COMP = 0,1,2,3

class FFormat:
  UNKNOWN = 0

def my_open(config, filepath, mode):
  if config.use_gzip:
    return gzip.open(filepath, mode)
  return open(filepath, mode)

def tmp_stage_name(config):
  return os.path.join(config.stage_dir, str(uuid.uuid1()))

def int_to_date(int_date):
  return datetime.date(year  = int_date // 10000,
                       month = (int_date//100) % 100,
                       day   = int_date % 100)

def offset_from_today_to_int(count_dates):
  dd = datetime.date.today() - datetime.timedelta(days=count_dates)
  return dd.year * 10000 + dd.month * 100 + dd.day

def get_lerp_max(config):
  return 2**(config.int_len - 1) - 1

def dformat_to_nptype(config, dformat):
  if dformat == DFormat.RAW:
    return np.float64
  if config.int_len == 64:
    return np.int64
  return np.int32

def intlen_to_nptype(config):
  if config.int_len == 64:
    return np.int64
  return np.int32

def dict_to_np_pairs(dico, key_t, val_t):
  dtype=[('key', key_t), ('val', val_t)]
  return np.fromiter(dico.items(), dtype=dtype, count=len(dico))

def parse_args (help_msg):
  parser = argparse.ArgumentParser(help_msg)
  parser.add_argument ('--stage-dir', '-s', 
                        help='Directory for staging temp result files',
                        default='/tmp')
  parser.add_argument ('--raw-input', '-i', 
                        help='Gzipped file containing input dataframes')
  parser.add_argument ('--col-name', '-c', 
                        help='The name of the series to process',
                        default='$CHIST')
  parser.add_argument ('--prob-output', '-p', 
                        help='Output file containing the probability distribution')
  parser.add_argument ('--use-gzip',
                        help='Use gzip to store results in staging (can make the program core)',
                        action='store_true',
                        default=False)
  parser.add_argument ('--int-len',
                        help='The len of each normalized price in bits',
                        default=32)
  parser.add_argument ('--alphabet-len',
                        help='The number of symbols inside the alphabet for prob distribution',
                        default=64)
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

##############################################################################

__init_log__()

