import requests
import os
import json
from typing import Dict, Any, Optional

# --- Constants ---
# It is highly recommended to set your API key as an environment variable
# Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
# Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
_GEMINI_API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
_GEMINI_MODEL_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash"


def get_model_metadata(api_key: str) -> Optional[Dict[str, Any]]:
    """
    Fetches and returns metadata for the specified Gemini model.

    Args:
        api_key: Your Google Gemini API key.

    Returns:
        A dictionary containing the model metadata, or None if an error occurs.
    """
    print("\n--- Fetching Model Data ---")
    params = {"key": api_key}
    try:
        response = requests.get(_GEMINI_MODEL_ENDPOINT, params=params)
        response.raise_for_status()  # Raise HTTPError for bad responses (4xx or 5xx)
        model_data = response.json()
        print("Model metadata retrieved successfully.")
        return model_data
    except requests.exceptions.HTTPError as e:
        print(f"HTTP error occurred: {e.response.status_code} - {e.response.text}")
    except requests.exceptions.ConnectionError as e:
        print(f"Connection error occurred: {e}")
    except requests.exceptions.Timeout as e:
        print(f"Timeout error occurred: {e}")
    except requests.exceptions.RequestException as e:
        print(f"An unexpected request error occurred: {e}")
    except json.JSONDecodeError as e:
        print(f"Failed to decode JSON response for model metadata: {e}")
    return None


def generate_gemini_content(
    api_key: str,
    text_prompt: str,
    system_instruction: str,
    generation_config: Dict[str, Any],
    enable_grounding: bool = False
) -> Optional[Dict[str, Any]]:
    """
    Sends a request to the Gemini API to generate content.

    Args:
        api_key: Your Google Gemini API key.
        text_prompt: The main text prompt for the model.
        system_instruction: The system instruction for the model.
        generation_config: A dictionary containing generation parameters like temperature, topP, topK,
                           thinkingBudget, and includeThoughts.
        enable_grounding: If True, enables Google Search grounding for the request.

    Returns:
        A dictionary containing the API response data, or None if an error occurs.
    """
    print("Sending Request to Gemini API...")
    headers = {
        "x-goog-api-key": api_key,
        "Content-Type": "application/json"
    }

    request_payload = {
        "system_instruction": {
            "parts": [
                {"text": system_instruction}
            ]
        },
        "contents": [
            {
                "parts": [
                    {"text": text_prompt}
                ]
            }
        ],
        "generationConfig": generation_config
    }

    if enable_grounding:
        request_payload["tools"] = [{"google_search": {}}]

    try:
        response = requests.post(_GEMINI_API_ENDPOINT, headers=headers, json=request_payload)
        response.raise_for_status()  # Raise HTTPError for bad responses (4xx or 5xx)
        response_data = response.json()
        print("Request successful.")
        return response_data
    except requests.exceptions.HTTPError as e:
        print(f"HTTP error occurred: {e.response.status_code} - {e.response.text}")
    except requests.exceptions.ConnectionError as e:
        print(f"Connection error occurred: {e}")
    except requests.exceptions.Timeout as e:
        print(f"Timeout error occurred: {e}")
    except requests.exceptions.RequestException as e:
        print(f"An unexpected request error occurred: {e}")
    except json.JSONDecodeError as e:
        print(f"Failed to decode JSON response from content generation: {e}")
    return None


def display_api_response(response_data: Dict[str, Any]) -> None:
    """
    Prints the generated text and any grounding metadata from the API response.

    Args:
        response_data: The dictionary containing the API response.
    """
    print("\n--- Response Data ---")

    if not response_data or "candidates" not in response_data or not response_data["candidates"]:
        print("No valid response data or candidates found.")
        return

    # Extract the generated text from the structured response
    generated_parts = response_data["candidates"][0].get("content", {}).get("parts", [])

    print("Generated Text:\n")
    if generated_parts:
        for part in generated_parts:
            if "text" in part:
                print(part["text"])
                print("-" * 50)  # Optional: Add a separator for clarity between different text parts
    else:
        print("No text generated.")

    # Check if grounding metadata is present in the response
    if "groundingMetadata" in response_data["candidates"][0]:
        grounding_metadata = response_data["candidates"][0]["groundingMetadata"]

        print("\n--- Google Grounding Info ---")

        # Print search queries
        if "webSearchQueries" in grounding_metadata:
            print("Search Queries:")
            for query in grounding_metadata["webSearchQueries"]:
                print(f"  - {query}")

        # Print source links
        if "groundingChunks" in grounding_metadata:
            print("\nSource Links:")
            for chunk in grounding_metadata["groundingChunks"]:
                if "web" in chunk and "uri" in chunk["web"]:
                    print(f"  - {chunk['web']['uri']}")

        print("-" * 50)


def main():
    """
    Main function to run the Gemini API interaction example.
    """
    api_key = os.getenv("GEMINI_API_KEY")
    if not api_key:
        print("Error: GEMINI_API_KEY environment variable not set.")
        print("Please set it before running the script (e.g., export GEMINI_API_KEY='YOUR_API_KEY').")
        return

    # --- Getting the Model Data section ---
    model_data = get_model_metadata(api_key)

    if model_data:
        print("\n--- Default Model Data Before Any Changes ---")
        for key, value in model_data.items():
            print(f"{key}: {value}")
    else:
        print("Could not retrieve model data. Exiting.")
        return

    input("\nPress any key to continue...\n")

    # --- Requesting a response section ---
    text_prompt = """
Explain the concept of LLM embeddings in simple terms.
"""

    system_instruction = """
You are a helpful AI assistant with a flourishing mindset.
"""

    # Generation configuration parameters
    generation_config = {
        "temperature": 1,
        "topP": 0.95,
        "topK": 64,
        "thinkingConfig": {
            "thinkingBudget": -1,  # Set to -1 for no budget, or a positive integer for a specific budget
            "includeThoughts": False
        }
    }

    enable_grounding = False  # Set to True to enable Google Search grounding

    print("\nSummary of values set for this request")
    print("---------------------------------------")
    print(f"text_prompt: {text_prompt.strip()}")
    print(f"system_instruction: {system_instruction.strip()}")
    print(f"temperature: {generation_config['temperature']}")
    print(f"topP: {generation_config['topP']}")
    print(f"topK: {generation_config['topK']}")
    print(f"thinking_budget: {generation_config['thinkingConfig']['thinkingBudget']}")
    print(f"include_thoughts: {generation_config['thinkingConfig']['includeThoughts']}")
    print(f"enable_grounding: {enable_grounding}")
    print("---------------------------------------")


    response_data = generate_gemini_content(
        api_key=api_key,
        text_prompt=text_prompt,
        system_instruction=system_instruction,
        generation_config=generation_config,
        enable_grounding=enable_grounding
    )

    if response_data:
        display_api_response(response_data)
    else:
        print("\nFailed to get a valid response from the Gemini API.")


if __name__ == "__main__":
    main()