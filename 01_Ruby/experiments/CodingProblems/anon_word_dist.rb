# frozen_string_literal: true

# Not known who this was asked by
# Find an efficient algorithm to find the smallest distance (measured in number of words) between any two given
# words in a string. For example, given words "hello", and "world" and a text content of
# "dog cat hello cat dog dog hello cat world", return 1 because there's only one word "cat" in between the two words.

# This method splits the text into an array of words, then iterates through the array while keeping track of the indices
# where word1 and word2 appear. It calculates the distance each time both words have been found and keeps the smallest
# distance encountered. The distance is measured in the number of words between the two words,
# not including the words themselves.

def smallest_distance(text, first_word, second_word)
  words = text.split
  first_index = second_index = nil
  min_distance = words.size

  words.each_with_index do |word, index|
    if word == first_word
      first_index = index
      min_distance = [min_distance, (second_index - first_index).abs - 1].min if second_index
    elsif word == second_word
      second_index = index
      min_distance = [min_distance, (first_index - second_index).abs - 1].min if first_index
    end
  end

  min_distance
end

# Example usage:
t = 'dog cat hello cat dog dog hello cat world'
w1 = 'hello'
w2 = 'world'
puts smallest_distance(t, w1, w2) # Output: 1
t = 'spam spam spam egg spam spam spam chips'
w1 = 'egg'
w2 = 'chips'
puts smallest_distance(t, w1, w2) # Output: 3
