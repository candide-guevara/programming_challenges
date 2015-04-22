import logging, os, re
from common import Common
from machineParts import *

logger = logging.getLogger(__name__)

# The file input must have the following format
# Any line starting by a '#' is a comment
#
# A line describing the input vector is structured as follows:
#   -IN-> input1 [, input2 ... ]
#
# A line describing a transition is structured as follows:
#   state (<input tape 1|wilcard>, ...) => nextState (<output tape 1|wilcard>, ...)(<move tape 1>, ...)
#
# A line describing the START transition is structured as follows:
#   START => nextState (<output tape 1|wilcard>, ...)(<move tape 1>, ...)
#
# A line describing an HALT transition is structured as follows:
#   state (<input tape 1|wilcard>, ...) => HALT
class ProgramParser (object):
  STATE_LINE = re.compile('^\s*(\w+)\s*\((.*)\)\s*=>\s*(\w+)\s*\((.*)\)\s*\((.*)\)')
  START_LINE = re.compile('^\s*START\s*=>\s*(\w+)\s*\((.*)\)\s*\((.*)\)')
  HALT_LINE  = re.compile('^\s*(\w+)\s*\((.*)\)\s*=>\s*HALT')
  INPUT_LINE = re.compile('^\s*-IN->\s*(.*)')
  SPLIT_ARGS = re.compile('\s*,\s*')
  EMPTY_OR_COMMENT = re.compile('^\s*#|^\s*$')

  def __init__ (self):
    self.transition = None
    self.inData = None
    self.filteredCount = 0

  def parse (self, programFile):
    logger.info("Parsing program file at %r", programFile)

    self.inData = []
    self.transition = TransitionFunction()

    for line in self.lineIterator(programFile):
      match = ProgramParser.STATE_LINE.search(line)
      if (match):
        self.parseTransition(match)
        continue
      match = ProgramParser.HALT_LINE.search(line)
      if (match):
        self.parseHaltTransition(match)
        continue
      match = ProgramParser.START_LINE.search(line)
      if (match):
        self.parseStartTransition(match)
        continue
      match = ProgramParser.INPUT_LINE.search(line)
      if (match):
        self.parseInputData(match)
        continue
      assert False, "Syntax error at line : %r" % line  
    
    self.transition.validate()
    logger.info("Parsing done useful lines read %d", self.filteredCount)
  ###END parse

  def lineIterator (self, programFile):
    self.filteredCount = 0
    assert os.path.isfile(programFile), "File does not exists"

    with open(programFile, 'r') as file:
      for line in file:
        match = ProgramParser.EMPTY_OR_COMMENT.search(line)
        if (not match):
          self.filteredCount += 1
          yield line
  ###END lineIterator        

  def parseStartTransition (self, match):
    logger.debug("Detected a START transition line %r", match.string)
    nextState = match.group(1)
    nextData = match.group(2).strip()
    nextMove = match.group(3).strip()

    key = TransitionKey.buildStartKey(Common.START_STATE_NAME)
    value = self.buildTransistionValue(nextState, nextData, nextMove)
    self.transition.add(key, value)

  def parseHaltTransition (self, match):
    logger.debug("Detected an HALT transition line %r", match.string)
    state = match.group(1)#.strip()
    curData = match.group(2).strip()

    key = self.buildTransistionKey(state, curData)
    value = TransitionValue.buildEndTransition(Common.HALT_STATE_NAME)
    self.transition.add(key, value)

  def parseTransition (self, match):
    logger.debug("Detected a transition line %r", match.string)
    state = match.group(1)
    curData = match.group(2).strip()
    nextState = match.group(3)
    nextData = match.group(4).strip()
    nextMove = match.group(5).strip()

    assert state and nextState, "Syntax error : current or next state are void"
    key = self.buildTransistionKey(state, curData)
    value = self.buildTransistionValue(nextState, nextData, nextMove)
    self.transition.add(key, value)
  
  def parseInputData (self, match):
    logger.debug("Detected an input line %r", match.string)
    assert not self.inData, "Program should only contain 1 input data"
    items = ProgramParser.SPLIT_ARGS.split(match.group(1).strip())
    for item in items:
      if (item): self.inData.append(item)
  
  def buildTransistionKey (self, state, data):
    dataList = []
    if (data):
      items = ProgramParser.SPLIT_ARGS.split(data)
      dataList.extend(items)
    return TransitionKey(state, dataList)  

  def buildTransistionValue (self, state, data, move):
    dataList = []
    moveList = []
    if (data):
      items = ProgramParser.SPLIT_ARGS.split(data)
      dataList.extend(items)
    if (move):
      items = ProgramParser.SPLIT_ARGS.split(move)
      moveList.extend(items)
    return TransitionValue(state, dataList, moveList)  

###END ProgramParser

class MachineRunner (object):

  def __init__ (self):
    self.parser = ProgramParser()

  def createAndRun (self, programFile):
    self.parser.parse(programFile)
    machine = TuringMachine(self.parser.transition, self.parser.inData)

    logger.info('Starting execution of turing machine with input : %r', self.parser.inData)
    logger.debug('Transition function : %r', self.parser.transition)
    self.executeMachine(machine)
    logger.info('Execution ended with output : %r', machine.getOutput())
 
  def executeMachine (self, machine):
    while(not machine.hasHalted()):
      logger.debug("Machine state : %r", machine)
      key = machine.buildNextKey()
      machine.next(key)

###END MachineRunner

