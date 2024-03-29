// Define a method to convert Cartesian (x,y) coordinates to Polar
polar := method(x, y,
    theta := y atan2(x)             // Compute the angle
    r := (x * x + y * y) sqrt       // Compute the distance
    list(r, theta)                  // The last expression is the return value
)

// Example usage
x := 4
y := 2
polarCoordinates := polar(x, y) 
distance := polarCoordinates at(0) asString exSlice(0, 6)           // experimenting with number formatting as well here
angle := polarCoordinates at(1) asString exSlice(0, 6)              // by very basic - converting to string and slicing 

# Display the result
writeln("The cartesian coordinates of X = ", x, " and Y = ", y, " is a distance of ", distance, " and angle of ", angle, " Radians as polar coordinates")
