# This problem was asked by Google.
# Given an array of numbers and an index i, return the index of the nearest larger number of the number 
# at index i, where distance is measured in array indices.
# For example, given [4, 1, 3, 5, 6] and index 0, you should return 3.
# If two distances to larger numbers are the equal, then return any one of them. 
# If the array at i doesn't have a nearest larger integer, then return null.
# Follow-up: If you can preprocess the array, can you do this in constant time?
def nearest_larger(arr, idx)
  left, right = idx - 1, idx + 1
  while left >= 0 || right < arr.length
    if left >= 0 && arr[left] > arr[idx]
      return left
    end
    if right < arr.length && arr[right] > arr[idx]
      return right
    end
    left -= 1
    right += 1
  end
  nil
end
  
# Example usage:
array = [4, 1, 3, 5, 6]
index = 0
puts nearest_larger(array, index) # Output should be 3

# Regarding the follow-up question, achieving constant time complexity for this problem would require 
# preprocessing the array. One approach could be to create a separate data structure, like a stack, 
# to keep track of potential candidates for the nearest larger number as you iterate through the array. 
# However, this preprocessing step would have its own time complexity, and the retrieval of the nearest 
# larger number index would be constant only after this preprocessing is done.