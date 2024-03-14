# frozen_string_literal: true

# This problem was asked by Apple.
# Gray code is a binary code where each successive value differ in only one bit, as well as when
# wrapping around. Gray code is common in hardware so that we don't see temporary spurious values
# during transitions.
# Given a number of bits n, generate a possible gray code for it.
# For example, for n = 2, one gray code would be [00, 01, 11, 10].

def gray_code(number_bits)
  return %w[0 1] if number_bits == 1

  prev_gray = gray_code(number_bits - 1)
  front = prev_gray.map { |code| "0#{code}" } # Add a '0' bit to the front of each code in the previous sequence
  back = # Add a '1' bit to the front of each code in the reversed previous sequence
    prev_gray.reverse.map do |code|
      "1#{code}"
    end
  front + back
end

# Example usage:
n = 2
puts "Gray code for #{n} bits is: #{gray_code(n).inspect}"
