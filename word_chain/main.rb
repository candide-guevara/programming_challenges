require_relative 'color_log'
require_relative 'word_containers'
require_relative 'word_distance'
require_relative 'word_chain_solver'

$mylog = initLogging("word_chain")

def main ()
  $mylog.info("Start word chain solver")

  dico = Dictionary.new "english_words"
  solver = ShortestChainSolver.new dico, Distance
  solver.solve 'monkey', 'mister'
  #solver.solve 'cookie', 'knight'

  $mylog.info("End word chain solver")
end

main()

