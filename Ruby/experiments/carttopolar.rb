# Define a method to convert Cartesian (x,y) coordinates to Polar
def polar(x,y)
    theta = Math.atan2(y,x)     # Compute the angle
    r = Math.hypot(x,y)         # Compute the distance
    [r, theta]                  # The last expression is the return value
end

# Here's how we use this method with parallel assignment
x = 4
y = 2
distance, angle = polar(x,y)

# puts "The cartesian coordinates of X = #{x} and Y = #{y} is a distance of #{distance} and angle of #{angle} Radians as polar coordinates"
puts "The cartesian coordinates of X = %.i and Y = %.i is a distance of %.4f and angle of %.4f Radians as polar coordinates" % [x, y, distance, angle]