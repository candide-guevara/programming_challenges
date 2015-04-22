require_relative "color_log"
require 'set'

MAX_WORD_LEN = 60
MIN_WORD_LEN = 2

class Dictionary
  attr_accessor :range

  def initialize (word_filename=nil)
    @range = (MIN_WORD_LEN .. MAX_WORD_LEN)
    @buckets = @range.map { Array.new() }
    loadWordFile word_filename if word_filename != nil
  end

  def getBucket (len)
    raise 'Bad word length' unless @range === len
    @buckets[len-MIN_WORD_LEN]
  end  

  def loadWordFile (word_filename)
    file = open(word_filename, "r")
    begin
      file.each_line { |line| loadSingleWord(line) }
    ensure 
      file.close
    end
    $mylog.debug "After loading file '#{word_filename}' : #{self}"
  end

  def loadSingleWord (word)
    word.chomp! "\n"
    return if word.length < MIN_WORD_LEN
    raise "Word is too long to fit #{word}" if word.length > MAX_WORD_LEN
    raise "This is not a word : #{word}" unless /^[a-záéíóú]+$/ =~ word

    bucket = getBucket(word.length)
    previous = bucket[-1]
    raise "The input file is not ordered : #{previous}->#{word}" if previous != nil && word < previous

    bucket << word
  end

  def length ()
    @buckets.inject(0) { |total,bucket| total += bucket.length }
  end

  def to_s ()
    histo = @range.zip(@buckets).map { |idx,bucket| "#{idx}:#{bucket.length}" }
    "Dictionary(len=#{length}, 
    #{histo.join ", "})"
  end
  
  def iterator (wordLen = nil)
    if wordLen != nil
      bucket = getBucket(wordLen)
      return bucket.each
    else
      return DictionaryIterator.new self
    end
  end

end

class WordListByDistance

  def initialize()
    @cache = {}
  end
  def clear()

    @cache = {}
  end
  
  def add (word, distance)
    @cache[word] = distance
  end

  def has (word)
    distance = @cache[word]
    return distance == nil ? false : distance
  end

  def load (dico, word, metric, distance)
    it = dico.iterator(word.length)
    it.each { |item| @cache[item] = distance if distance == metric.calculate(word,item) }
  end

  def iterator ()
    @cache.each
  end

  def length ()
    @cache.length
  end  

  def to_s ()
    @cache.to_s
  end  

end

class WordDistanceQueue
  
  class InnerPair
    attr_accessor :distance, :word
    def initialize (word, distance)
      @word = word
      @distance = distance
    end
    def <=> (other)
      @distance <=> other.distance
    end
    def inspect ()
      "(#{@word}, #{@distance})"
    end  
  end

  def initialize ()
    @queue = SortedSet.new
  end  

  def push (word, distance)
    @queue.add InnerPair.new(word, distance)
  end
  def pop ()
    result = @queue.first
    @queue.delete result
    return result
  end

  def length ()
    @queue.length
  end  
  def to_s ()
    @queue.to_a.inspect
  end  

end

