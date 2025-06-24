# old_processor.py
from typing import List

# --- Qodo Experiment 6: Code Refactoring/Enhancement ---
# Objective: Explore Qodo's ability to suggest code improvements and refactorings.
# Steps:
# 1. Open this file in your IDE.
# 2. Place your cursor anywhere inside the 'get_status_message' function.
# 3. Look for a "Refactor", "Improve Code", or "Code Suggestions" option from Qodo
#    (often appearing as a code lens above the function under Options or in a right-click context menu). Click it.
# 4. Observe Qodo's suggestion (it should propose using a dictionary mapping for status codes
#    to make the function more concise and readable). Review and apply the suggestion.
# 5. Repeat steps 2-4 for the 'calculate_sum_of_squares_loop' function below. Qodo might suggest
#    using a more Pythonic list comprehension or generator expression with 'sum()'.
# --------------------------------------------------------

def get_status_message(status_code: int) -> str:
    """
    Returns a status message based on the status code.
    """
    if status_code == 200:
        return "OK"
    elif status_code == 400:
        return "Bad Request"
    elif status_code == 401:
        return "Unauthorized"
    elif status_code == 404:
        return "Not Found"
    elif status_code == 500:
        return "Internal Server Error"
    else:
        return "Unknown Status"

def calculate_sum_of_squares_loop(numbers: List[int]) -> int:
    """Calculates sum of squares using a loop."""
    total = 0
    for num in numbers:
        total += num * num
    return total