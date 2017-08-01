#! /bin/python3

import re
import glob
from util import Printer, Stat
from grid_solver import *

class BatchSolver:
   
  def __init__(self, fileNames, solver):
    self.fileNames = fileNames
    self.solver = solver
  
  def loadGrids(self):
    regex = re.compile('\d')
    grids = []
    for name in self.fileNames:
      with open(name, 'r') as file:
        Printer.myprint("Loading {0}".format(name), ' ... ')
        grid = [[int(s) for s in regex.findall(line)] for line in file.readlines() if line.strip() is not "" ]
        grids.append(grid)
    return grids
  
  def solveSingleGrid(self, grid):
    stat = Stat()
    self.stats.append(stat)
    Printer.myprint('Solving : ', grid=grid)
    solvedGrid = self.solver.solve(grid, stat)

    if(solvedGrid == None): Printer.myprint('Grid could not be solved!', level=Printer.WARN)
    else:
      Printer.myprint('Solution found :', level=Printer.INFO, grid=solvedGrid)
      Printer.myprint(level=Printer.WARN, stat=stat)
  
  def batchSolve(self):
    grids = self.loadGrids();
    self.stats = []
    for grid in grids: self.solveSingleGrid(grid)
    globalStat = Stat.calculateGlobal(self.stats)
    Printer.myprint('All done :', level=Printer.ERROR, stat=globalStat)


GRID_DIR = 'resource/grids'
PREFIXES = ('regular', 'easy', 'medium', 'hard')
fileNames = [name for prefix in PREFIXES for name in glob.glob(GRID_DIR + '/' + prefix + '*')]

batch = BatchSolver(fileNames, OptSolver())
batch.batchSolve()
Printer.flush()

