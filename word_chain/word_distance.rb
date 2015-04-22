require_relative "color_log"

class Distance
  INFINITY = 1024
  
  def self.calculate (left, right)
    return INFINITY if left.length != right.length
    left.each_char.zip(right.each_char).inject(0) { |dist,chars| dist += chars[0] != chars[1] ? 1 : 0 }
  end

end

