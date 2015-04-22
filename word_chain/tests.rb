require_relative 'color_log'
require_relative 'word_containers'
require_relative 'word_distance'
require_relative 'word_iterators'
require_relative 'word_chain_solver'
require 'test/unit'

$mylog = initLogging("word_chain") unless $mylog != nil

class TestContainers < Test::Unit::TestCase
  
  def testLoadWordsInDictionary () 
    dico = Dictionary.new
    dico.loadSingleWord('choco')
    dico.loadSingleWord('pops')

    begin
      dico.loadSingleWord('76sdf')
      assert false, "Should not load non word strings"
    rescue; end  
    begin
      dico.loadSingleWord('y' * (MAX_WORD_LEN + 1))
      assert false, "Should not load too long words"
    rescue; end  
    begin
      dico.loadSingleWord('choci')
      assert false, "Words to be loaded must be ordered"
    rescue; end  

    $mylog.debug "Result : #{dico}"
    assert dico.length == 2, "Added too many words in Dictionary"
  end

  def testDictionaryIteration ()
    dico = Dictionary.new
    ['choc', 'choco', 'chocolat', 'pops', 'salut'].each {|w| dico.loadSingleWord(w) }
    
    it5 = dico.iterator(5)
    assert it5.all? { |i| %w[choco salut].include? i }, 'Iterator returned unexpected result'

    it = dico.iterator
    assert it.all? { |i| %w[choc pops choco salut chocolat].include? i }, 'Whole iterator did not return all dico items'

    it8 = dico.iterator(8)
    it8.next
    begin 
      it8.next; 
      assert false, "Wrong number of elements"
    rescue; end
  end

  def testWordByDistanceList ()
    dico = Dictionary.new
    list = WordListByDistance.new
    %w[chimp choco salut thing].each {|w| dico.loadSingleWord(w) }

    list.load(dico, 'champ', Distance, 1)
    assert list.has('chimp') && list.length == 1, 'Distance between words not respected'

    list.clear
    list.load(dico, 'chimp', Distance, 3)
    assert list.has('choco') && list.has('thing') && list.length == 2, 'Distance between words not respected'
  end  

  def testWordCache ()
    cache = WordListByDistance.new
    cache.add('choco', Distance.calculate('choco', 'rufus'))
    assert cache.has('choco'), 'Could not store word on cache'
    assert cache.has('salut') == false, 'Cache returns false positives'
  end  

  def testWordQueue ()
    queue = WordDistanceQueue.new
    queue.push 'chimp', Distance.calculate('choco', 'chimp')
    queue.push 'choco', Distance.calculate('choco', 'choco')
    queue.push 'rufi', Distance.calculate('choco', 'rufi')
    queue.push 'salut', Distance.calculate('choco', 'salut')
    assert queue.pop.word == 'choco' && queue.pop.word == 'chimp' && queue.pop.word == 'salut' && queue.pop.word == 'rufi', 'Bad queueing order'
  end  

end

class TestDistance < Test::Unit::TestCase

  def testSimpleCharDistance ()
    dist = Distance
    assert dist.calculate('choco', 'choco') == 0, "Bad distance choco/choco"
    assert dist.calculate('choco', 'choko') == 1, 'Bad distance choco/choko'
    assert dist.calculate('choco', 'choc') == Distance::INFINITY, 'Bad distance choco/choc'
    assert dist.calculate('choc', 'pops') == 4, 'Bad distance choc/pops'
  end

end

class TestWordIterators < Test::Unit::TestCase

  def setup()
    @dico = Dictionary.new
    %w[chimp champ chomp china lead lean loan load goal goad goat gold ].sort!.each { |w| @dico.loadSingleWord(w) }
  end

  def testUnorderedIterator ()
    unorderIt = UnorderedIterator.new @dico, 4
    allWords = @dico.iterator(4).to_a
    assert unorderIt.all? { |w| allWords.include? w }, 'Iterator does not return all elements'
  end

  def testBipolarIteration ()
    it = BiPolarIt.new Distance, @dico, 'champ', 'china'
    assert it.next.word == 'chimp', 'Iterator does not respect right bound distance'
    assert it.next.word == 'chomp', 'Iterator must return all word of distance 1 from left bound'
    begin
      it.next; assert False, 'Iterator returned too many elements'
    rescue; end  
  end

end

class TestWordChainSolvers < Test::Unit::TestCase

  def setup()
    @dico = Dictionary.new
    %w[chimp champ chomp china lead lean loan load road toad goad gold ].sort!.each { |w| @dico.loadSingleWord(w) }
  end

  def validateWordChain (solution, metric, left, right)
    return false if solution[0] != left || solution[-1] != right
    last = nil
    solution.all? do |word|
      stepOk = true
      unless last == nil
        stepOk = metric.calculate(last,word) == 1 
      end
      last = word
      stepOk
    end
  end

  def validateCompleteSolution (solution, metric)
    left = solution['left']
    right = solution['right']
    shortest = solution['shortest']
    raise "Solution object badly formed" if left == nil || right == nil || shortest == nil

    solution['all'].all? do |chain|
      $mylog.debug "Validating #{chain}"
      chainOk = shortest.length <= chain.length
      chainOk = validateWordChain(chain, metric, left, right) if chainOk
    end
  end

  def tryOutTrivialScenarios (solver)
    assert solver.solve('choco', 'choco'), "Could not find an existing trivial solution"
    assert solver.solve('choco', 'choko'), "Could not find an existing trivial solution"
    assert solver.solve('chimp', 'china') == false, "Found an impossible solution"
  end

  def testChainValidator ()
    solution = {
      "left" => "lead", "right" => "gold", 
      "shortest" => ["lead", "load", "goad", "gold"], 
      "all" => [["lead", "load", "goad", "gold"], ["lead", "load", "toad", "goad", "gold"], ["lead", "load", "toad", "road", "goad", "gold"], ["lead", "load", "road", "goad", "gold"], ["lead", "load", "road", "toad", "goad", "gold"], ["lead", "lean", "loan", "load", "goad", "gold"], ["lead", "lean", "loan", "load", "toad", "goad", "gold"], ["lead", "lean", "loan", "load", "toad", "road", "goad", "gold"], ["lead", "lean", "loan", "load", "road", "goad", "gold"], ["lead", "lean", "loan", "load", "road", "toad", "goad", "gold"]]
    }

    assert validateWordChain(['choco', 'choko', 'rufus'], Distance, 'choco', 'rufus') == false, "Validate solution is wrong"
    assert validateCompleteSolution(solution, Distance), "Solution validator is wrong"

    solution['shortest'] = ["lead", "load", "toad", "goad", "gold"]
    assert validateCompleteSolution(solution, Distance) == false, "Solution validator is wrong"
  end

  def genericWordChainTests (solver, totalSolutions=nil)
    $mylog.info "Performing test for solver : #{solver.class}"
    tryOutTrivialScenarios solver

    assert solver.solve('lead', 'gold'), "Could not find an existing solution"
    solution = solver.solution
    assert validateCompleteSolution(solution, Distance), "Found a bad solution : #{solution}"
    if totalSolutions != nil
      assert solution['all'].length == totalSolutions, "Did not find all the possible solutions"
    end  
  end

  def testWordChainSolvers ()
    solver = AnySolutionChainSolver.new @dico, Distance
    genericWordChainTests solver, 1

    solver = AllPossibleChainsSolver.new @dico, Distance
    genericWordChainTests solver, 10

    solver = ShortestChainSolver.new @dico, Distance
    genericWordChainTests solver
  end

end

