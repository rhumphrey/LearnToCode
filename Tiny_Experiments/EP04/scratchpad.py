# scratchpad.py (or any empty Python file)

# --- Qodo Experiment 9: Generate a Small Code Snippet (using Qodo Chat) ---
# Objective: Use Qodo's chat interface to generate new code based on natural language prompts.
# Steps:
# 1. Open an empty Python file (like this one), or just place your cursor in any Python file.
# 2. Open the Qodo chat panel.
# 3. In the chat input, type a request for a code snippet, for example:
#    "Generate a Python function that checks if a string is a palindrome, ignoring case and spaces."
#    OR "Write a Python class 'BankAccount' with methods for deposit, withdrawal, and balance inquiry."
# 4. Qodo will generate the code directly into the chat. You can then typically copy/paste it,
#    or Qodo might offer to insert it into your current file.
# --------------------------------------------------------------------------
def is_palindrome(s: str) -> bool:
    """
    Check if the given string is a palindrome, ignoring case and spaces.

    Args:
        s (str): The input string to check.

    Returns:
        bool: True if the string is a palindrome, False otherwise.
    """
    # Remove spaces and convert to lowercase
    cleaned = ''.join(c.lower() for c in s if not c.isspace())
    # Check if cleaned string is equal to its reverse
    return cleaned == cleaned[::-1]