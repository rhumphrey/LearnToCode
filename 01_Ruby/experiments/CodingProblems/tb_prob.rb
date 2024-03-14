# frozen_string_literal: true

# This problem was asked by Triplebyte.
# You are given n numbers as well as n probabilities that sum up to 1. Write a function to generate one of the numbers
# with its corresponding probability.
# For example, given the numbers [1, 2, 3, 4] and probabilities [0.1, 0.5, 0.2, 0.2], your function should return 1
# 10% of the time, 2 50% of the time, and 3 and 4 20% of the time.
# You can generate random numbers between 0 and 1 uniformly.

# This function first checks if the input arrays are of the same length and if the probabilities sum up to 1.
# It then creates an array of cumulative probabilities which helps in determining the range within which the
# generated random number falls. The function finally returns the corresponding number based on the generated
# random number.
# Remember, due to the nature of randomness, you’ll need to run the function multiple times to observe
# the distribution aligning with the probabilities.

def weighted_random(numbers, probabilities)
  raise 'The arrays must be of the same length' if numbers.length != probabilities.length
  raise 'The sum of probabilities must be 1' if probabilities.sum != 1

  cumulative_probabilities = probabilities.each_with_index.map do |_prob, index|
    probabilities[0..index].sum
  end
  random_number = rand

  cumulative_probabilities.each_with_index do |prob, index|
    return numbers[index] if random_number < prob
  end
end

# Example usage:
numbers = [1, 2, 3, 4]
probabilities = [0.1, 0.5, 0.2, 0.2]
puts weighted_random(numbers, probabilities)
