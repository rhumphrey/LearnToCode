# frozen_string_literal: true

# This test runs the weighted_random function 10,000 times and records the results.
# It then checks that the results are within a reasonable delta of the expected frequencies.

require 'minitest/autorun'
require_relative 'tb_prob' # method from tb_prob.rb - weighted_random

# The WeightedRandomTest class is a test suite for testing the weighted_random method.
# It inherits from Minitest::Test, which is a framework for unit tests in Ruby.
#
# The setup method initializes the instance variables used in the tests.
# @numbers - An array of numbers to be used as possible outcomes.
# @probabilities - An array of probabilities corresponding to each number in @numbers.
# @results - A hash to keep track of the number of times each number is returned by the weighted_random method.
#
# The test_weighted_random method simulates calling the weighted_random method 10,000 times
# and then checks if the results are within the expected probability distribution.
#
# The simulate_weighted_random method simulates the weighted random selection process.
# @param times [Integer] The number of times to simulate the method call.
#
# The assert_results_delta method checks if the results are within an acceptable delta range.
# It iterates over the expected_deltas hash and uses the assert_in_delta method to verify the results.
#
class WeightedRandomTest < Minitest::Test
  def setup
    @numbers = [1, 2, 3, 4]
    @probabilities = [0.1, 0.5, 0.2, 0.2]
    @results = Hash.new(0)
  end

  def test_weighted_random
    simulate_weighted_random(10_000)
    assert_results_delta
  end

  private

  def simulate_weighted_random(times)
    times.times do
      result = weighted_random(@numbers, @probabilities)
      @results[result] += 1
    end
  end

  def assert_results_delta
    expected_deltas = { 1 => 1000, 2 => 5000, 3 => 2000, 4 => 2000 }
    expected_deltas.each do |number, expected|
      assert_in_delta expected, @results[number], 300
    end
  end
end
