/*
1. Factorial:
   - Description: The factorial of a non-negative integer `N` (denoted as `N!`) is the product of all positive integers from 1 to `N`.
   - Base Case: `factorial(0, 1)` defines that the factorial of 0 is 1.
   - Recursive Case: `factorial(N, Result)` calculates the factorial of `N` by multiplying `N` with the factorial of `(N-1)`.
   - Example Usage:
     - Query: `factorial(5, X).`
     - Result: `X = 120`.
*/
factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, SubResult),
    Result is N * SubResult.


/*
2. Fibonacci Series:
   - Description: The Fibonacci series starts with 0 and 1, and each subsequent number is the sum of the two preceding ones.
   - Base Cases: `fibonacci(0, 0)` and `fibonacci(1, 1)` define the first two terms.
   - Recursive Case: `fibonacci(N, Result)` calculates the `N`th Fibonacci number by summing the `(N-1)`th and `(N-2)`th Fibonacci numbers.
   - Example Usage:
     - Query: `fibonacci(7, X).`
     - Result: `X = 13`.
*/
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, Result) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    Result is F1 + F2.


/*
3. Sum of Natural Numbers:
   - Description: The sum of the first `N` natural numbers can be computed recursively.
   - Base Case: `sum_natural(0, 0)` defines that the sum of 0 natural numbers is 0.
   - Recursive Case: `sum_natural(N, Result)` calculates the sum by adding `N` to the sum of the first `(N-1)` natural numbers.
   - Example Usage:
     - Query: `sum_natural(4, X).`
     - Result: `X = 10`.
*/
sum_natural(0, 0).
sum_natural(N, Result) :-
    N > 0,
    N1 is N - 1,
    sum_natural(N1, SubResult),
    Result is N + SubResult.
