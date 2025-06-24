# buggy_list_operations.py
from typing import List

# --- Qodo Experiment 4: Refining Tests with a Failed Test ---
# Objective: See how Qodo assists in identifying and fixing bugs via failing tests.
# Steps:
# 1. Open this file in your IDE.
# 2. Place your cursor inside the 'find_max_value_in_list' function.
# 3. Use Qodo's "Generate Tests" feature.
# 4. Run the generated tests (Qodo often provides a way to run them directly or with pytest).
# 5. Observe that at least one test (e.g., one with a specific number sequence) should fail
#    due to the off-by-one bug in the loop.
# 6. Look for Qodo's suggestion to "Fix Code" or "Fix Test" near the failing test or
#    the function definition itself. Click it.
# 7. Observe Qodo's proposed fix for the 'for' loop (it should suggest 'range(len(numbers))'
#    or a similar correct iteration).
# 8. Apply the fix and re-run the tests to confirm that all tests now pass.
# ------------------------------------------------------------

def find_max_value_in_list(numbers: List[int]) -> int:
    
    if not numbers:
        raise ValueError("List cannot be empty.")
    max_val = numbers[0]
    
    for i in range(len(numbers) - 1):
        if numbers[i] > max_val:
            max_val = numbers[i]
    return max_val