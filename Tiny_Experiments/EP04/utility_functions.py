# utility_functions.py
import math

# --- Qodo Experiment 7: Docstring Generation ---
# Objective: See how Qodo can automatically generate documentation for your code.
# Steps:
# 1. Open this file in your IDE.
# 2. Place your cursor inside or directly above the 'calculate_area_of_circle' function.
# 3. Find Qodo's "Generate Docstring" option (often a code lens or in a right-click context menu). Click it.
# 4. Observe the generated docstring being suggested in the chat window so you can insert any documentation.
# 5. Repeat for the 'UserProfile' class and its methods below to see class and method docstring generation.
# -------------------------------------------------

def calculate_area_of_circle(radius: float) -> float:
    # This function is missing a docstring!    
    return math.pi * radius ** 2

class UserProfile:
    def __init__(self, username: str, email: str):
        self.username = username
        self.email = email

    def get_user_info(self) -> str:
        return f"Username: {self.username}, Email: {self.email}"