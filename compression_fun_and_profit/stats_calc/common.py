import logging, sys, os, re, argparse, gzip

def my_open(config, filepath, mode):
  if config.use_gzip:
    return gzip.open(filepath, mode)
  return open(filepath, mode)

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
                        default=True)
  parser.add_argument ('--int-len',
                        help='The len of each normalized price in bits',
                        default=32)
  args = parser.parse_args()
  return args

