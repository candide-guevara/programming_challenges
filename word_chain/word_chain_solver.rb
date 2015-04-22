require_relative "color_log"
require_relative 'word_distance'
require_relative 'word_containers'
require_relative 'word_iterators'

class AnySolutionChainSolver
  
  def initialize (dico, metric)
    @dico = dico
    @metric = metric
  end

  def solution
    @solution
  end

  def init_before_solving (left, right)
    @branch = [left]
    @iterations = 0
    @solution = { 
      'left' => left, 'right' => right,
      'shortest' => nil, 
      'all' => [],
    }
  end

  def solve (left, right)
    init_before_solving left, right

    $mylog.info "Solving #{left} -> #{right}"
    solved = trivial_case(left, right)
    solved = perform_iteration(left, right) unless solved

    $mylog.info "Solved #{left} -> #{right} ? #{solved}"
    $mylog.info "Iterations needed #{@iterations}, result : #{@solution}"
    return solved
  end

  def trivial_case (left, right)
    case @metric.calculate(left, right)
      when 0
        store_branch_as_solution
        return true
      when 1
        @branch << right
        store_branch_as_solution
        return true
      when Distance::INFINITY   
        raise "Cannot solve a chain of infinite length"
      else
        return false
    end
  end

  def store_branch_as_solution ()
    $mylog.debug "One Solution found : #{@branch}"
    @solution['shortest'] = Array.new( @branch )
    @solution['all'] << Array.new( @branch )
  end

  def detect_end_condition (left, right)
    dist = @metric.calculate(left, right)
    raise "INVARIANT : left and right are always different" if dist == 0

    if dist == 1
      @branch << right
      store_branch_as_solution
      @branch.pop
      return true
    end
    return false
  end

  def prune_branch_if_possible (left, right, pair)
    word = pair.word
    if @branch.include? word
      $mylog.debug "detected chain cycle #{word} / #{@branch}"
      return true 
    end
    return false
  end

  def keep_iterating_at_this_level (left, right, oneSolutionFound)
    not oneSolutionFound
  end

  def perform_iteration (left, right)
    $mylog.debug "#{@branch}: Partially solving #{left} -> #{right}"
    return true if detect_end_condition(left, right)

    oneFromLeftList = BiPolarIt.new @metric, @dico, left, right 
    oneSolutionFound = false

    for pair in oneFromLeftList
      @iterations += 1
      raise "INVARIANT : left and right have at least distance 2" if @metric.calculate(left,right) < 1
      next if prune_branch_if_possible(left, right, pair)

      word = pair.word
      @branch << word
      oneSolutionFound = true if perform_iteration(word, right)
      @branch.pop

      break unless keep_iterating_at_this_level(left, right, oneSolutionFound)
    end
    return oneSolutionFound
  end

end

class AllPossibleChainsSolver < AnySolutionChainSolver

  def solution
    raise "Found several times the same solution" if @solution['all'].uniq! != nil
    shortest = nil

    @solution['all'].each do |chain| 
      shortest = chain if shortest == nil || shortest.length > chain.length
    end

    @solution['shortest'] = shortest
    return @solution
  end

  def store_branch_as_solution ()
    $mylog.debug "One Solution found : #{@branch}"
    @solution['all'] << Array.new( @branch )
  end

  def keep_iterating_at_this_level (left, right, oneSolutionFound)
    true
  end

end

class ShortestChainSolver < AnySolutionChainSolver

  def store_branch_as_solution ()
    $mylog.debug "One Solution found : #{@branch}"
    chain = Array.new( @branch )
    shortest = @solution['shortest']

    @solution['all'] << chain
    if shortest == nil || shortest.length > chain.length
      @solution['shortest'] = chain
      @shortestLen = chain.length
    elsif shortest != nil  
      raise "INVARIANT : Should not find a longer solution : #{chain}"
    end  
  end

  def init_before_solving (left, right)
    @shortestLen = nil
    super
  end

  def prune_branch_if_possible (left, right, pair)
    if @shortestLen != nil
      if (pair.distance + @branch.length + 1) >= @shortestLen
        $mylog.debug "better_chain_len = branch_len(#{@branch.length}) + word_dis(#{pair.distance}) + 1"
        return true
      end

      if @branch.length > 1
        if @metric.calculate(pair.word, @branch[-2]) < 2
          $mylog.debug "Useless move : #{@branch}-#{pair.word}-?-#{right}"
          return true
        end
      end
    end
    return super
  end

  def keep_iterating_at_this_level (left, right, oneSolutionFound)
    if @shortestLen != nil
      if (@branch.length + 2) >= @shortestLen
        $mylog.debug "This branch is already too long : #{@branch}-?-#{right} / shortest=#{@shortestLen}"
        return false
      end
    end
    return true
  end

end

