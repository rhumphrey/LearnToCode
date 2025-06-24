# feature_update.py
from typing import List

# --- Qodo Experiment 10: Simulated PR Review (Local) ---
# Objective: Simulate a code review scenario locally to see Qodo's review capabilities.
# Setup:
# 1. Ensure this file contains the 'Modified version' of the 'calculate_discount' function below.
#    (If you started with the 'Original version', simply update it to the 'Modified version').
# Steps:
# 1. Select the entire *modified* 'calculate_discount' function in your editor.
# 2. Open the Qodo chat panel.
# 3. Type a prompt like: "Review this change. What are the potential impacts of adding the
#    `apply_tax` parameter? Suggest improvements or missing tests."
# 4. Qodo will analyze the (conceptually) changed function and provide feedback similar
#    to a pull request review. It might point out the new parameter's effect, suggest
#    making the tax rate configurable, or highlight the need for new test cases covering 'apply_tax'.
# ---------------------------------------------------------

# Modified version for the experiment:
def calculate_discount(price: float, discount_percentage: float, apply_tax: bool = False) -> float:
    """
    Calculates discounted price, optionally applying a fixed tax.
    """
    if not (0 <= discount_percentage <= 100):
        raise ValueError("Discount percentage must be between 0 and 100.")

    discounted_price = price * (1 - discount_percentage / 100)

    if apply_tax:
        # A fixed 5% tax for simplicity
        tax_rate = 0.05
        discounted_price *= (1 + tax_rate)

    return discounted_price

# Original version (for conceptual comparison, not to be in file at same time during experiment):
# def calculate_discount(price: float, discount_percentage: float) -> float:
#     """Calculates discounted price."""
#     if not (0 <= discount_percentage <= 100):
#         raise ValueError("Discount percentage must be between 0 and 100.")
#     return price * (1 - discount_percentage / 100)