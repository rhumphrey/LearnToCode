# Define a method to calculate the mean of a list of numbers
def mean(list)
    sum = 0.0
    list.each do |num|
      sum += num
    end
    sum / list.length
end

# Define a method to calculate the standard deviation of a list of numbers
def standard_deviation(list)
    mean = mean(list)
    sum = 0.0
    list.each do |num|
      sum += (num - mean) ** 2
    end
    Math.sqrt(sum / list.length)
end

# Test the code with an example list
list = [1, 2, 3, 4, 5]
puts "The standard deviation of #{list} is #{standard_deviation(list)}"