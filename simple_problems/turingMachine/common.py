import logging, sys, re, os

class Common (object):
  level = logging.INFO
  MAX_MACHINE_ITERATION = 10 ** 6;
  MAX_TAPE_SIZE = 10 ** 4;
  NUMBER_OF_TAPES = 3

  BLANK_SYMBOL = '_'
  WILDCARD_SYMBOL = '*'
  DO_NOT_WRITE_SYMBOL = '*'
  MOVE_LEFT_SYMBOL, MOVE_RIGHT_SYMBOL, MOVE_STAY_SYMBOL = ('<', '>', '_')

  HALT_STATE_NAME = 'HALT'
  START_STATE_NAME = 'START'

  args = {'verbose'         : False,
          'help'            : False,
          'program_file'    : ''
          }

  @staticmethod
  def getArguments():
    for index in range(0, len(sys.argv)):
      arg = sys.argv[index]
      isLast = index == len(sys.argv)-1

      if (arg in ('-h', '--help')):
        Common.args['help'] = True
      elif (arg == '-v'):
        Common.args['verbose'] = True

      # Specific arguments
      elif (arg in ('-p', '--program') and not isLast):
        Common.args['program_file'] = sys.argv[index+1]
  ### END getArguments    

  @staticmethod
  def initLog ():
    blueFnt = '\033[94m'
    greenFnt = '\033[92m'
    resetFnt = '\033[0m'
    if (Common.args['verbose']):
      Common.level = logging.DEBUG
      if sys.version_info < (2, 6):
        format = '%(levelname)s-%(name)s::%(funcName)s-> %(message)s'
      else:
        format = '%s%%(levelname)s-%s%%(name)s::%%(funcName)s-> %s%%(message)s' % (blueFnt, greenFnt, resetFnt)
    else:
      format = '%(message)s'
    logging.basicConfig(level = Common.level, format = format)

# ## End Common

Common.getArguments()
Common.initLog()
logger = logging.getLogger(__name__)


