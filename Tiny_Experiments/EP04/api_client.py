# api_client.py
import requests
from typing import Dict, Any

# --- Qodo Experiment 8: Free-form Code Query (using Qodo Chat) ---
# Objective: Interact with Qodo using natural language to ask questions about your code.
# Steps:
# 1. Open this file in your IDE.
# 2. Select the entire 'fetch_data_from_api' function (from its 'def' line to its last 'return' line).
# 3. Open the Qodo chat panel (usually accessible via a dedicated sidebar icon or a command from the command palette).
# 4. In the chat input, type a context-aware question about the selected code, for example:
#    "Are there any potential security vulnerabilities in this code related to API requests?"
#    OR "How can I make this more robust against network issues?"
# 5. Observe Qodo's response, which should provide insights based on its analysis of the selected code.
# ------------------------------------------------------------------

def fetch_data_from_api(url: str, params: Dict[str, Any] = None) -> Dict[str, Any]:
    """Fetches data from a given API URL."""
    try:
        response = requests.get(url, params=params)
        response.raise_for_status()  # Raise an exception for HTTP errors
        return response.json()
    except requests.exceptions.HTTPError as http_err:
        print(f"HTTP error occurred: {http_err}")
        return {}
    except requests.exceptions.ConnectionError as conn_err:
        print(f"Connection error occurred: {conn_err}")
        return {}
    except requests.exceptions.Timeout as timeout_err:
        print(f"Timeout error occurred: {timeout_err}")
        return {}
    except requests.exceptions.RequestException as req_err:
        print(f"An error occurred: {req_err}")
        return {}

# Example usage:
# data = fetch_data_from_api("https://jsonplaceholder.typicode.com/todos/1")
# print(data)
