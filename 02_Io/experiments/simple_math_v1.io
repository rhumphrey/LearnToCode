// Experimenting with objects, messages and simple math for two numbers
TwoValues := Object clone do(
    firstNumber := 0
    secondNumber := 0
    sum := method(firstNumber + secondNumber)
    average := method(sum / 2)
    maximum := method(if(firstNumber > secondNumber, firstNumber, secondNumber))
    difference := method(if(firstNumber > secondNumber, firstNumber - secondNumber, secondNumber - firstNumber))
    product := method(firstNumber * secondNumber)
    quotient := method(if(secondNumber != 0, firstNumber / secondNumber, "Cannot divide by zero"))
    power := method(firstNumber ^ secondNumber)
    modulus := method(firstNumber % secondNumber)
    minimum := method(if(firstNumber < secondNumber, firstNumber, secondNumber))
    geometricMean := method((firstNumber * secondNumber) sqrt)
    harmonicMean := method(2 / (1 / firstNumber + 1 / secondNumber))
    euclideanDistance := method((firstNumber - secondNumber) abs)
)

myTwoValues := TwoValues clone

myTwoValues firstNumber = (File standardInput readLine("Please enter first whole number: ")) asNumber
myTwoValues secondNumber = (File standardInput readLine("Please enter second whole number: ")) asNumber

thisSum := myTwoValues sum
thisAverage := myTwoValues average
thisMaximum := myTwoValues maximum
thisDifference := myTwoValues difference
thisProduct := myTwoValues product
thisQuotient := myTwoValues quotient
thisPower := myTwoValues power
thisModulus := myTwoValues modulus
thisMinimum := myTwoValues minimum
thisGeometricMean := myTwoValues geometricMean
thisHarmonicMean := myTwoValues harmonicMean
thisEuclideanDistance := myTwoValues euclideanDistance

// Display the results
writeln("The sum is " .. thisSum)
writeln("The average is " .. thisAverage)
writeln("The highest value is " .. thisMaximum)
writeln("The difference is " .. thisDifference)
writeln("The product is " .. thisProduct)
writeln("The quotient is " .. thisQuotient)
writeln("The power is " .. thisPower)
writeln("The modulus is " .. thisModulus)
writeln("The minimum is " .. thisMinimum)
writeln("The geometric mean is " .. thisGeometricMean)
writeln("The harmonic mean is " .. thisHarmonicMean)
writeln("The Euclidean distance is " .. thisEuclideanDistance)
