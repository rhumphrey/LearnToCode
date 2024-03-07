# This problem was asked by Goldman Sachs
# Given a list of numbers L, implement a method sum(i, j) which returns the sum from the sublist 
# L[i:j] (including i, excluding j).
# For example, given L = [1, 2, 3, 4, 5], sum(1, 3) should return sum([2, 3]), which is 5.
# You can assume that you can do some pre-processing. sum() should be optimized over the pre-processing step.

class PrefixSumArray
  def initialize(nums)
    @prefix_sum = []
    sum = 0
    nums.each do |num|
      sum += num
      @prefix_sum << sum
    end
  end

  def sum(i, j)
    return @prefix_sum[j - 1] - (i.zero? ? 0 : @prefix_sum[i - 1])
  end
end
  
# Example usage:
L = [1, 2, 3, 4, 5]
prefix_sum_array = PrefixSumArray.new(L)
puts prefix_sum_array.sum(1, 3)                 # Output: 5  