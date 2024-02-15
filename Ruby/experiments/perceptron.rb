# Define the sigmoid activation function
def sigmoid(x)
    1 / (1 + Math.exp(-x))
end
  
# Define the perceptron function
def perceptron(inputs, weights, bias)
# Check that the inputs and weights have the same length
    if inputs.length != weights.length
        raise ArgumentError, "inputs and weights must have the same length"
    end
  
    # Compute the weighted sum of inputs and bias
    sum = bias
    inputs.each_with_index do |input, index|
        sum += input * weights[index]
    end
  
    # Apply the activation function and return the output
    sigmoid(sum)
end
  
# Here is an example of using the perceptron function to classify a point as either above or below a line
  
# Define the inputs, weights, and bias
inputs = [0.5, 0.7] # The x and y coordinates of the point
weights = [2, -3] # The slope and intercept of the line
bias = 1 # A constant term to shift the line
  
# Call the perceptron function and print the output
output = perceptron(inputs, weights, bias)
puts output
  
# Interpret the output as a binary class
if output >= 0.5
    puts "The point is above the line"
else
    puts "The point is below the line"
end  

# Define the inputs, weights, and bias
inputs = [0.5, 0.8] # x1 and x2
weights = [1.2, -0.4] # w1 and w2
bias = -0.2 # b

output = perceptron(inputs, weights, bias)
puts output
  
# Interpret the output as a binary class
if output >= 0.5
    puts "The point is above the line"
else
    puts "The point is below the line"
end  