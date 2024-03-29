// Define a method to calculate the mean of a list of numbers
mean := method(list,
    sum := 0
    list foreach(num, sum = sum + num)
    sum / list size
)

// Define a method to calculate the standard deviation of a list of numbers
standardDeviation := method(list,
    meanValue := mean(list)
    sum := 0
    list foreach(num, sum = sum + (num - meanValue) ** 2)
    (sum / list size) sqrt
)

// Test the code with an example list
testList := list(1, 2, 3, 4, 5)
writeln("The standard deviation of ", testList, " is ",standardDeviation(testList) )
