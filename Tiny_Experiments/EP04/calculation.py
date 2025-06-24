# calculations.py

# --- Qodo Experiment 2: Function with Edge Cases ---
# Objective: Test Qodo's ability to identify and generate tests for common edge cases.
# Steps:
# 1. Open this file in your IDE.
# 2. Place your cursor inside the 'divide' function.
# 3. Use Qodo's "Test this function" feature.
# 4. Observe the panel that appears to the left and walk through the 'chat' responding as necessary
# 5. Repeat steps 2-4 for the 'get_nth_fibonacci' function below.
#    Check for tests covering 'n=0', 'n=1', and especially 'n < 0' inputs,
#    expecting a ValueError.
# ----------------------------------------------------

def divide(numerator: float, denominator: float) -> float:
    """
    Divides numerator by denominator.
    Raises ValueError if denominator is zero.
    """
    if denominator == 0:
        raise ValueError("Cannot divide by zero")
    return numerator / denominator

def get_nth_fibonacci(n: int) -> int:
    """
    Calculates the nth Fibonacci number.
    Raises ValueError for negative input.
    """
    if n < 0:
        raise ValueError("Input cannot be negative.")
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b