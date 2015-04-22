require_relative "color_log"

module CreateEachFromNext

  def each ()
    return self unless block_given?
    loop do
      yield self.next
    end  
  end
  
end

class UnorderedIterator
  include CreateEachFromNext
  include Enumerable
  @@staticOrder = { 0 => [], 1 => [0] }

  def initialize(dico, len)
    @bucket = dico.getBucket(len)
    @cur_idx = 0
    @itOrder = buildIterationOrder(@bucket.length)
  end

  def buildIterationOrder(maxLen)
    unless @@staticOrder.include? maxLen
      @@staticOrder[maxLen] = (0..maxLen-1).to_a.shuffle!
      #$mylog.debug "Created new iteration order #{@@staticOrder}"
    end
    return @@staticOrder[maxLen]
  end

  def next ()
    raise StopIteration.new if @cur_idx >= @itOrder.length
    result = @bucket[ @itOrder[@cur_idx] ]
    @cur_idx += 1
    return result
  end

end

class DictionaryIterator
  include CreateEachFromNext
  include Enumerable

  def initialize(dico)
    @dico = dico
    @cur_idx = @dico.range.begin
    @cur_it = @dico.iterator(@cur_idx)
  end

  def next ()
    while true
      begin
        return @cur_it.next
      rescue StopIteration => err
        @cur_idx += 1
        raise err if @cur_idx > @dico.range.end
        @cur_it = @dico.iterator(@cur_idx)
      end  
    end
  end

end

class BiPolarIt
  include CreateEachFromNext
  include Enumerable
  LOOK_AHEAD = 512

  def initialize (metric, dico, left, right)
    @dicoIt = UnorderedIterator.new(dico, left.length)
    #@dicoIt = dico.iterator left.length
    @right = right
    @left = left
    @queue = WordDistanceQueue.new
    @metric = metric
  end

  def next ()
    if @queue.length < 1
      loop do
        word = @dicoIt.next
        if @metric.calculate(word, @left) == 1
          @queue.push(word, @metric.calculate(word, @right))
        end
      end
    end

    raise StopIteration.new if @queue.length < 1
    return @queue.pop
  end

  def next_lookahaed ()
    countLoops = 0
    loop do
      word = @dicoIt.next
      countLoops += 1

      if @metric.calculate(word, @left) == 1
        @queue.push(word, @metric.calculate(word, @right))
      end
      break if countLoops >= LOOK_AHEAD && @queue.length > 0
    end

    #$mylog.debug "Queue after iteration : #{@queue}"
    result = @queue.pop
    raise StopIteration.new if result == nil
    return result
  end

end

