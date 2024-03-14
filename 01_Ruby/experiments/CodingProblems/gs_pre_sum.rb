# frozen_string_literal: true

# This problem was asked by Goldman Sachs
# Given a list of numbers L, implement a method sum(i, j) which returns the sum from the sublist
# L[i:j] (including i, excluding j).
# For example, given L = [1, 2, 3, 4, 5], sum(1, 3) should return sum([2, 3]), which is 5.
# You can assume that you can do some pre-processing. sum() should be optimized over the pre-processing step.

# The PrefixSumArray class is designed to optimally calculate the sum of elements
# in a subarray of a given list of numbers. It utilizes a prefix sum array to store
# the cumulative sum of elements, allowing for constant-time retrieval of subarray sums.
#
# Example:
#   Given the list [1, 2, 3, 4, 5], the prefix sum array would be [1, 3, 6, 10, 15].
#   To find the sum of elements from index 1 to 3, we calculate prefix_sum[3 - 1] - prefix_sum[1 - 1],
#   which gives us 6 - 1 = 5, the sum of elements [2, 3].
#
# Initialization:
#   nums - An array of numbers for which the prefix sum array will be constructed.
#
# Methods:
#   sum(start_index, end_index) - Returns the sum of elements in the subarray starting
#                                 at 'start_index' and ending just before 'end_index'.
#                                 Both 'start_index' and 'end_index' should be within
#                                 the bounds of the original array.
#
# Usage:
#   prefix_sum_array = PrefixSumArray.new([1, 2, 3, 4, 5])
#   subarray_sum = prefix_sum_array.sum(1, 3) # Returns 5
#
class PrefixSumArray
  def initialize(nums)
    @prefix_sum = []
    sum = 0
    nums.each do |num|
      sum += num
      @prefix_sum << sum
    end
  end

  def sum(start_index, end_index)
    @prefix_sum[end_index - 1] - (start_index.zero? ? 0 : @prefix_sum[start_index - 1])
  end
end

# Example usage:
L = [1, 2, 3, 4, 5].freeze
prefix_sum_array = PrefixSumArray.new(L)
puts prefix_sum_array.sum(1, 3) # Output: 5
