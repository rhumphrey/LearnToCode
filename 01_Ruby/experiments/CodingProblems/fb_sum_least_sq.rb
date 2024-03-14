# frozen_string_literal: true

# This problem was asked by Facebook.
# Given a positive integer n, find the smallest number of squared integers which sum to n.
# For example, given n = 13, return 2 since 13 = 32 + 22 = 9 + 4.
# Given n = 27, return 3 since 27 = 32 + 32 + 32 = 9 + 9 + 9.
def find_minimum_squares(target_sum)
  min_squares = initialize_min_squares(target_sum)
  calculate_min_squares(min_squares, target_sum)
  min_squares[target_sum]
end

def initialize_min_squares(target_sum)
  Array.new(target_sum + 1, Float::INFINITY).tap { |arr| arr[0] = 0 }
end

def calculate_min_squares(min_squares, target_sum)
  1.upto(target_sum) do |current_number|
    min_squares[current_number] = min_squares_for_number(current_number, min_squares)
  end
end

def min_squares_for_number(current_number, min_squares)
  1.upto(Math.sqrt(current_number).to_i).map do |square_root|
    min_squares[current_number - square_root**2] + 1
  end.min
end


# Example usage
puts find_minimum_squares(13)  # Output: 2
puts find_minimum_squares(27)  # Output: 3
