import logging, itertools
from common import Common

logger = logging.getLogger(__name__)

class Tape (object):
  TYPE_INPUT, TYPE_OUTPUT, TYPE_WORK = (1, 2, 0)
  
  def __init__ (self, data = [], tapeType = None):
    self.type = tapeType or Tape.TYPE_WORK
    self.tape = []
    self.pos = 0
    self._mark = 0
    self.clean(data)

  def getData (self):
    # returns a copy of the data not a pointer
    return self.tape[:]

  def clean (self, data = []):
    self.tape = [Common.BLANK_SYMBOL]
    if (data):
      self.tape[:] = data
    self.pos = 0
    self._mark = 0

  def rewind (self):
    self.pos = self._mark

  def mark (self):
    self._mark = self.pos

  def read (self):
    assert self.pos >= 0, "Attempt to read on left edge of tape"
    assert self.type != Tape.TYPE_OUTPUT, "Cannot read an output tape"
    if (self.pos < len(self.tape)):
      return self.tape[self.pos]
    else:
      assert Common.MAX_TAPE_SIZE > self.pos, "Reached maximum tape length => ABORT !"
      return Common.BLANK_SYMBOL
  
  def write(self, item):
    if (item == Common.DO_NOT_WRITE_SYMBOL):
      return

    assert self.pos >= 0, "Attempt to read on left edge of tape"
    assert self.type != Tape.TYPE_INPUT, "Cannot write on an input tape"

    if (self.pos >= len(self.tape)):
      assert Common.MAX_TAPE_SIZE > self.pos, "Reached maximum tape length => ABORT !"
      self.tape.extend(itertools.repeat(Common.BLANK_SYMBOL, self.pos-len(self.tape)+1))
    self.tape[self.pos] = item

  def move (self, direction):
    if (direction == Common.MOVE_LEFT_SYMBOL):
      if (self.pos != 0):
        self.pos -= 1
    elif (direction == Common.MOVE_RIGHT_SYMBOL):
      self.pos += 1
    assert Common.MAX_TAPE_SIZE > self.pos, "Reached maximum tape length => ABORT !"
  
  def _read (self):
    assert self.pos >= 0, "Attempt to read on left edge of tape"
    if (self.pos < len(self.tape)):
      return self.tape[self.pos]
    else:
      return Common.BLANK_SYMBOL

  def _move(self, direction):
    if (direction == Common.MOVE_LEFT_SYMBOL and self.pos == 0):
      return Common.BLANK_SYMBOL
    self.move(direction)
    return self._read()
  
  def __repr__ (self):
    LEN_OF_DISPLAY = 10
    self.mark()

    left = [ str(self._move(Common.MOVE_LEFT_SYMBOL)) for i in range(LEN_OF_DISPLAY) ]
    left.reverse()
    left = "".join(left)
    self.rewind()
    right = "".join([ str(self._move(Common.MOVE_RIGHT_SYMBOL)) for i in range(LEN_OF_DISPLAY) ])
    self.rewind()
      
    lines = [ "Tape type : %r" % self.type,
              left + str(self._read()) + right,
              " " * len(left) + "^" ]
    return "\n".join(lines)

### END Tape

# IMPORTANT : the input data must be ordered as follows [ IN_TAPE, WORK_TAPE_1, ... WORK_TAPE_N ]
class TransitionKey (object):
  STATE_START, STATE_END, STATE_PROCESS = (1, 2, 0)
  
  def __init__ (self, state, inputData = [], stateType = None):
    if (not inputData):
      inputData.extend(itertools.repeat(Common.WILDCARD_SYMBOL, Common.NUMBER_OF_TAPES-1))
    assert len(inputData) == Common.NUMBER_OF_TAPES-1, "Input data length must match the number of readable tapesi : %r" % inputData
    
    self.type = stateType or TransitionKey.STATE_PROCESS
    self.state = state
    self.data = tuple(inputData)

  @classmethod
  def buildStartKey (klass, state):
    return klass(state, [], TransitionKey.STATE_START)

  # Not optimal, it will produce huge buckets ...
  def __hash__ (self):
    return self.state.__hash__()

  def __eq__ (self, other):
    if (self.state == other.state):
      #print("#################### %r == %r" % (self, other))
      for myItem, otherItem in zip(self.data, other.data):
        if (myItem != Common.WILDCARD_SYMBOL and otherItem != Common.WILDCARD_SYMBOL and myItem != otherItem):
          return False
      return True      
    return False
  
  def getData (self, idx = None):
    if (idx == None):
      return self.data
    else :
      assert idx < Common.NUMBER_OF_TAPES-1, "This tape number does not exists"
      return self.data[idx]

  def getType (self, newType = None):
    return self.type
  def getState (self):
    return self.state

  def __repr__ (self):
    return "(state=%r, data=%r)" % (self.getState(), self.getData())

### END TransitionKey

# IMPORTANT : the output data must be ordered as follows [ OUT_TAPE, WORK_TAPE_1, ... WORK_TAPE_N ]
# IMPORTANT : the head move must be ordered as follows [ IN_TAPE, OUT_TAPE, WORK_TAPE_1, ... WORK_TAPE_N ]
class TransitionValue (TransitionKey):

  def __init__ (self, state, data = [], moves = [], stateType = TransitionKey.STATE_PROCESS):
    if (not data):
      data.extend(itertools.repeat(Common.DO_NOT_WRITE_SYMBOL, Common.NUMBER_OF_TAPES-1))
    assert len(data) == Common.NUMBER_OF_TAPES-1, "Output data length must match the number of writable tapes"
    super().__init__(state, data, stateType)

    self.move = []
    if (not moves):
      moves.extend(itertools.repeat(Common.MOVE_STAY_SYMBOL, Common.NUMBER_OF_TAPES))
    assert len(moves) == Common.NUMBER_OF_TAPES, "Tape move length must match the number of tapes"
    self.move[0:Common.NUMBER_OF_TAPES] = moves
      
  @classmethod
  def buildFromTransitionKey (klass, key, moves = []):
    return klass(key.getState(), key.getData(), moves, key.getType())

  @classmethod
  def buildEndTransition (klass, state):
    key = TransitionKey(state, [], TransitionKey.STATE_END)
    return klass.buildFromTransitionKey(key)

  def getMove (self, idx = None):
    if (idx == None):
      move = self.move[:]
      return move
    else :
      assert idx < Common.NUMBER_OF_TAPES, "This tape number does not exists"
      return self.move[idx]

  def __repr__ (self):
    return "(state=%r, data=%r, move=%r)" % (self.getState(), self.getData(), self.getMove())

### END TransitionValue

class TransitionFunction (object):

  def __init__ (self):
    self.function = {}
    self.startKey = None

  def clean (self):
    self.startKey = None
  
  def validate (self):
    assert len(self.function), "There are not transitions for this function"
    assert self.startKey, "No START state has been declared"
    return True

  def add (self, key, value):
    assert key and value, "You must provide valid in and out states" 
    if (key.getType() == TransitionKey.STATE_START):
      assert not self.startKey , "Cannot add 2 START states to the function"
      self.startKey = key
    
    assert not key in self.function, "Key clash new=%r, previous=%r" % (key, self.function[key])
    logger.debug("Adding line : %r => %r", key, value)
    self.function[key] = value

  def next (self, key):
    assert key in self.function, "The key %r does not exists for the function" % key

    value = self.function[key]
    logger.debug("Next value : %r => %r", key, value)
    return value

  def __repr__ (self):
    lines = [ "Transition function count=%d" % len(self.function) ]
    lines += [ str(key) + " => " + str(value) for key, value in self.function.items() ]
    return "\n".join(lines)

### END TransitionFunction

class TuringMachine (object):
  
  def __init__ (self, transition, inData = []):
    assert Common.NUMBER_OF_TAPES > 2, "The turing machine must have at least 3 tapes"
    self.inTape = Tape(inData, Tape.TYPE_INPUT)
    self.outTape = Tape([], Tape.TYPE_OUTPUT)
    self.workTapes = [ Tape() for i in range(Common.NUMBER_OF_TAPES-2) ]

    self.transition = transition
    self.stepCount = 0
    self.currentValue = None

  def clean (self, inData = None):
    logger.debug("Cleaning the machine for a new execution")
    allWriteTapes = itertools.chain([self.outTape], self.workTapes)
    for tape in allTapes: tape.clean()

    if (inData):
      self.inTape = Tape(inData, Tape.TYPE_INPUT)
    self.transition.clean()
    self.stepCount = 0
    self.currentValue = None
    
  def getOutput (self):
    return self.outTape.getData()

  def hasHalted (self):
    return self.currentValue and self.currentValue.getType() == TransitionKey.STATE_END

  def buildNextKey (self):
    if (not self.currentValue):
      return self.transition.startKey
    
    state = self.currentValue.getState()
    newType = self.currentValue.getType()
    allReadTapes = itertools.chain([self.inTape], self.workTapes)
    data = [ tape.read() for tape in allReadTapes ]
    return TransitionKey(state, data, newType)

  def next (self, nextKey):
    assert not self.hasHalted(), "Cannot transition from END state"
    assert self.stepCount < Common.MAX_MACHINE_ITERATION, "Max step count reached => ABORT !"

    self.stepCount += 1
    self.currentValue = self.transition.next(nextKey)
    self.writeTapes()
    self.moveTapeHeads()

  def writeTapes (self):
    allWriteTapes = itertools.chain([self.outTape], self.workTapes)
    for item, tape in zip(self.currentValue.getData(), allWriteTapes):
      tape.write(item)

  def moveTapeHeads (self):
    allTapes = itertools.chain([self.inTape], [self.outTape], self.workTapes)
    for direction, tape in zip(self.currentValue.getMove(), allTapes):
      tape.move(direction)

  def __repr__ (self):
    lines = [ "Turing machine: steps=%d, halted=%r" % (self.stepCount, self.hasHalted()),
              "Current transition value=%r" % self.currentValue,
              "In tape : %r" % self.inTape, 
              "Out tape : %r" % self.outTape,
              "Work tapes : "] 
    lines += [ str(tape) for tape in self.workTapes ]
    return "\n".join(lines)

### END TuringMachine

