def bubble_sort(array)
  n = array.length                                  # Get the length of the array
  # Loop through the array n-1 times
  (n-1).times do
    # Loop through the array from index 0 to n-2
    (0...n-1).each do |i|
      if array[i] > array[i+1]                      # Compare the current element with the next element
        array[i], array[i+1] = array[i+1], array[i] # Swap them if the current element is larger
      end
    end
  end
  array                                             # Return the sorted array
end

numbers = [1,6,3,7,3,9,5,7,3,5]
print numbers
bubble_sort(numbers)
puts
print numbers