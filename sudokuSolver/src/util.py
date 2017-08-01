class Printer:
  buf = []
  DEBUG = 2
  INFO = 3
  WARN = 4
  ERROR = 5
  PRINT_LEVEL = 3

  def myprint(*mes, level=DEBUG, grid=None, stat=None):
    if level < Printer.PRINT_LEVEL: return
    if mes: Printer.buf.extend(mes)
    if grid: Printer.buf.extend(Printer._printGrid(grid))
    if stat: Printer.buf.extend(Printer._printStat(stat))

  def flush():
    print("\n".join(Printer.buf))
    Printer.buf = []

  def _printGrid(grid):
    formatStr = "{} " * len(grid)
    return [formatStr.format(*line) for line in grid]

  def _printStat(stat):
    average = stat.candidates / stat.recursion if stat.recursion else 0
    strStat = ["Statistics :", 
      "Recursion depth : {0}".format(stat.recursion),
      "Candidates : {0}".format(stat.candidates),
      "Average : {0}".format(average)
    ]
    return strStat

class Stat:

  def __init__(self):
    self.recursion = 0
    self.candidates = 0

  def calculateGlobal(stats):
    stat = Stat()
    for s in stats:
      stat.recursion += s.recursion
      stat.candidates += s.candidates
    return stat


