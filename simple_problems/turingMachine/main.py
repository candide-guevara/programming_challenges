import logging, sys
from common import Common
from machineRunner import *

logger = logging.getLogger(__name__)

HELP_MESSAGE = '''3-tape Turing machine emulator :
  -h    Help message
  -v    Verbose log output
  -p    Program file to emulate
'''

### MAIN ###

if __name__ == '__main__':
  logger.debug('Command line args = %r', Common.args)
  if (Common.args['help']):
    logger.info(HELP_MESSAGE)
    sys.exit()

MachineRunner().createAndRun(Common.args['program_file'])
logger.info("ALL DONE !!!")

