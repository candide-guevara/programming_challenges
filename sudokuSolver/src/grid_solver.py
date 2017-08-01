import time
from bin.python.util import Printer

class SimpleSolver:

  FULL_CANDIDATES = set(range(1,10))

  def solve(self, grid, stat):
    self.initialCalculations(grid)
    if not self.validGrid(): return None
    if not self.recursiveSolve(stat): return None
    return self.grid

  def initialCalculations(self, grid):
    self.grid = [grid[i][:] for i in range(len(grid))]
    if '_gen' in vars(self): del self._gen

  def validGrid(self):
    rowSet = set()
    colSet = set()
    for i in range(9):
      for j in range(9):
        x = self.grid[i][j]
        y = self.grid[j][i]
        if x and x in rowSet: return None
        if y and y in colSet: return None
        rowSet.add(x)
        colSet.add(y)
      rowSet.clear()
      colSet.clear()
    return 1

  def yieldMove(self):
    for i in range(9): 
      for j in range(9):
        if not self.grid[i][j]: yield (i,j)

  def getNextMove(self):
    if '_gen' not in vars(self): self._gen = self.yieldMove()
    return next(self._gen, None)

  def performMove(self, move, candidate):
    self.grid[move[0]][move[1]] = candidate

  def undoMove(self, move):
    self.grid[move[0]][move[1]] = 0
    self._gen = self.yieldMove()

  def getNumberCandidates(self, move):
    nSet = set(self.grid[move[0]])
    for j in range(9): nSet.add(self.grid[j][move[1]])
    return sorted(SimpleSolver.FULL_CANDIDATES - nSet)

  def recursiveSolve(self, stat):
    stat.recursion+=1
    move = self.getNextMove()
    if not move: return 1
    candidates = self.getNumberCandidates(move)

    for c in candidates:
      stat.candidates+=1
      self.performMove(move, c)
      if self.recursiveSolve(stat): return 1
      stat.recursion-=1
      self.undoMove(move)
    return 0

class OptSolver(SimpleSolver):
  
  def initialCalculations(self, grid):
    self.grid = [grid[i][:] for i in range(len(grid))]
    self.xSets = [SimpleSolver.FULL_CANDIDATES.copy() for i in range(9)]
    self.ySets = [SimpleSolver.FULL_CANDIDATES.copy() for i in range(9)]
    for i in range(9):
      for j in range(9):
        self.xSets[i].discard(self.grid[i][j])
        self.ySets[i].discard(self.grid[j][i])

  def getNextMove(self):
    minLen = 10
    self.candidates = []
    self.move = None
    for i in range(9):
      for j in range(9):
        if not self.grid[i][j]: 
          tCand = self.xSets[i] & self.ySets[j]
          tLen = len(tCand)
          if tLen < minLen:
            minLen = tLen
            self.move = (i,j)
            self.candidates = tCand
    self.candidates = sorted(self.candidates)
    #Printer.PRINT_LEVEL = 1
    #Printer.myprint("Move ", str(self.move), "Candidates ", str(self.candidates), grid=self.grid)
    #Printer.flush()
    #time.sleep(5)
    return self.move

  def performMove(self, move, candidate):
    self.grid[move[0]][move[1]] = candidate
    self.xSets[move[0]].discard(candidate)
    self.ySets[move[1]].discard(candidate)

  def undoMove(self, move):
    self.xSets[move[0]].add(self.grid[move[0]][move[1]])
    self.ySets[move[1]].add(self.grid[move[0]][move[1]])
    self.grid[move[0]][move[1]] = 0

  def getNumberCandidates(self, move):
    if 1 < len(self.candidates):
      shifts = move[0] % (move[1]+1)
      for i in range(shifts): 
        tmp = self.candidates.pop()
        self.candidates.insert(0, tmp)
    return self.candidates

