# Not know who this was asked by
# Given a list, sort it using this method: reverse(lst, i, j), which reverses lst from i to j.
# Method Definition: The method reverse(lst, i, j) is defined with three parameters: 
# lst (the list to be modified), i (the starting index of the segment to be reversed), 
# and j (the ending index of the segment to be reversed).
# Reversing the Segment: Inside the method, lst[i..j] creates a sub-array from the list lst that 
# includes all elements from index i to index j. The .reverse method is then called on this sub-array, 
# which reverses the order of the elements within it.
# Updating the List: The original list lst is then updated with the reversed sub-array 
# in place of the original segment from i to j. This is done by assigning the reversed sub-array 
# back to lst[i..j].
# Returning the List: Finally, the method returns the modified list lst with the specified segment reversed.
# In the context of the sorting algorithm, this reverse method is used to swap adjacent elements 
# if they are out of order, contributing to the overall sorting process. 
# The method is repeatedly called until the entire list is sorted. 
# This approach is a variation of the bubble sort algorithm, where instead of swapping individual elements, 
# segments of the list are reversed.


def reverse(lst, i, j)
  lst[i..j] = lst[i..j].reverse
  lst
end
  
def sort_with_reverse(lst)
  sorted = false
  until sorted
    sorted = true
    (lst.length - 1).times do |i|
      if lst[i] > lst[i + 1]
        lst = reverse(lst, i, i + 1)
        sorted = false
      end
    end
  end
  lst
end
  
# Example usage:
list = [3, 2, 1, 4, 5, 7, 6]
sorted_list = sort_with_reverse(list)
puts sorted_list

# Using built-in reverse
def sort_with_reverse_bi(lst)
  sorted = false
  until sorted
    sorted = true
    (lst.length - 1).times do |i|
      if lst[i] > lst[i + 1]
        # Reverse the elements in place
        lst[i..i + 1] = lst[i..i + 1].reverse!
        sorted = false
      end
    end
  end
  lst
end
  
# Example usage:
list = [3, 2, 1, 4, 5, 7, 6]
sorted_list = sort_with_reverse_bi(list)
puts sorted_list
  