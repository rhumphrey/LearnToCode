safeDivide := method(numerator, denominator,
    if(denominator != 0,
        numerator / denominator,
        "Error: Division by zero is not allowed."
    )
)

a := 10

// Example usage:
safeDivide(10, 2) println  // Output: 5
safeDivide(10, 3) println  // Output: 3.3333333333333335
safeDivide(10, 0) println  // Output: Error: Division by zero is not allowed.

// Note: here is what you would get if you try 10/0 in Io 
(10 / 0) println            // Output: 1.#INF00e+000