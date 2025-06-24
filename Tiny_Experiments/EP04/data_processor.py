# data_processor.py
from typing import List, Dict, Any

# --- Qodo Experiment 3: Function with Docstrings/Type Hints ---
# Objective: Observe if Qodo uses code documentation to generate more intelligent tests.
# Steps:
# 1. Open this file in your IDE.
# 2. Place your cursor inside the 'process_data' function.
# 3. Use Qodo's "Test this function" feature and interact with the Qodo Gen 'chat'.
# 4. Observe the generated tests. Qodo should leverage the information from the docstrings
#    and type hints to create more specific and relevant test cases (e.g., testing with
#    different 'value_type' arguments like 'str', 'int', and handling missing keys).
# 5. (Optional, for comparison) Remove the docstrings and type hints from this function,
#    regenerate tests, and compare the difference in test quality to see the impact of
#    good documentation on Qodo's output.
# --------------------------------------------------------------

def process_data(data: List[Dict[str, Any]], key: str, value_type: type) -> List[Any]:
    """
    Extracts values from a list of dictionaries based on a given key.

    Args:
        data: A list of dictionaries to process.
        key: The key whose values are to be extracted.
        value_type: The expected type of the values.

    Returns:
        A list of extracted values, filtered by the specified type.
    """
    extracted_values = []
    for item in data:
        if key in item and isinstance(item[key], value_type):
            extracted_values.append(item[key])
    return extracted_values