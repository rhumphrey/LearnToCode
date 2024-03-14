# frozen_string_literal: true

# This problem was asked by MongoDB.
# Given a list of elements, find the majority element, which appears more than half the time (> floor(len(lst) / 2.0)).
# You can assume that such element exists.
# For example, given [1, 2, 1, 1, 3, 4, 1 ,1], return 1.
def majority_element(lst)
  counts = Hash.new(0)
  lst.each do |num|
    counts[num] += 1
  end
  counts.each do |num, count|
    return num if count > (lst.length / 2.0).floor
  end
end

# Example usage:
list = [1, 2, 1, 1, 3, 4, 1, 1]
puts majority_element(list)  # Output: 1
list = [1, 2, 3, 3, 3, 3, 3, 1]
puts majority_element(list)  # Output: 3
