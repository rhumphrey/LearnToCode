import requests
import os
import json
from typing import Dict, Any, Optional

# --- Constants ---
# It is highly recommended to set your API key as an environment variable
# Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
# Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
_GEMINI_API_ENDPOINT: str = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
_GEMINI_MODEL_ENDPOINT: str = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash"


def get_model_metadata(api_key: str) -> Optional[Dict[str, Any]]:
    """
    Fetches and returns metadata for the specified Gemini model.

    This function makes a GET request to the Gemini model endpoint to retrieve
    information about the model, such as its capabilities, input/output
    formats, and pricing.

    Args:
        api_key: Your Google Gemini API key. This key is used to authenticate
                 your request to the Gemini API.

    Returns:
        A dictionary containing the model metadata if the request is successful.
        Returns None if any error (HTTP, connection, timeout, or JSON decoding)
        occurs during the request.
    """
    print("\n--- Fetching Model Data ---")
    params: Dict[str, str] = {"key": api_key}
    try:
        response: requests.Response = requests.get(_GEMINI_MODEL_ENDPOINT, params=params)
        # Raise an HTTPError for bad responses (4xx or 5xx)
        response.raise_for_status()
        model_data: Dict[str, Any] = response.json()
        print("Model metadata retrieved successfully.")
        return model_data
    except requests.exceptions.HTTPError as e:
        # Handles errors such as 401 Unauthorized, 403 Forbidden, 404 Not Found, 500 Internal Server Error.
        print(f"HTTP error occurred: {e.response.status_code} - {e.response.text}")
    except requests.exceptions.ConnectionError as e:
        # Handles network-related errors (e.g., DNS failure, refused connection).
        print(f"Connection error occurred: {e}")
    except requests.exceptions.Timeout as e:
        # Handles request timeouts.
        print(f"Timeout error occurred: {e}")
    except requests.exceptions.RequestException as e:
        # Catches any other general requests-related errors.
        print(f"An unexpected request error occurred: {e}")
    except json.JSONDecodeError as e:
        # Handles cases where the response body is not a valid JSON.
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
    Sends a request to the Gemini API to generate content based on the provided inputs.

    This function constructs a JSON payload with the user's prompt, system instruction,
    and generation parameters, then sends it to the Gemini API endpoint. It also
    handles the optional inclusion of Google Search grounding.

    Args:
        api_key: Your Google Gemini API key.
        text_prompt: The main text prompt (user's query) for the model.
                     Example: "Explain the concept of LLM embeddings in simple terms."
        system_instruction: The instruction that sets the behavior or persona
                            of the model. Example: "You are a helpful AI assistant."
        generation_config: A dictionary containing parameters that control the
                           generation process, such as:
                           - "temperature": Controls randomness (0.0-1.0).
                           - "topP": Nucleus sampling probability.
                           - "topK": Top-k sampling.
                           - "thinkingConfig": A sub-dictionary for thought process control:
                               - "thinkingBudget": Time budget for thinking (-1 for no budget).
                               - "includeThoughts": Whether to include the model's
                                                    thought process in the response.
        enable_grounding: If True, enables Google Search grounding. This allows
                          the model to use real-time information from Google Search
                          to generate more accurate and up-to-date responses.

    Returns:
        A dictionary containing the parsed JSON response data from the Gemini API
        if the request is successful. Returns None if any error occurs during
        the request or JSON decoding.
    """
    print("Sending Request to Gemini API...")
    headers: Dict[str, str] = {
        "x-goog-api-key": api_key,
        "Content-Type": "application/json"
    }

    # Construct the base request payload
    request_payload: Dict[str, Any] = {
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

    # Conditionally add the tools for Google Search grounding if enabled
    if enable_grounding:
        request_payload["tools"] = [{"google_search": {}}]

    try:
        response: requests.Response = requests.post(_GEMINI_API_ENDPOINT, headers=headers, json=request_payload)
        # Raise an HTTPError for bad responses (4xx or 5xx)
        response.raise_for_status()
        response_data: Dict[str, Any] = response.json()
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
    Prints the generated text and any associated grounding metadata from the API response.

    This function parses the Gemini API response dictionary and extracts the
    generated text content. If grounding was enabled and the response contains
    grounding metadata (e.g., web search queries, source links), it prints
    that information as well.

    Args:
        response_data: The dictionary containing the parsed API response from
                       the `generate_gemini_content` function.
    """
    print("\n--- Response Data ---")

    # Check if the response data is valid and contains candidates
    if not response_data or "candidates" not in response_data or not response_data["candidates"]:
        print("No valid response data or candidates found in the API response.")
        return

    # Extract the generated text from the first candidate in the structured response
    # Using .get() with a default empty dictionary/list to safely access nested keys
    generated_parts: list = response_data["candidates"][0].get("content", {}).get("parts", [])

    print("Generated Text:\n")
    if generated_parts:
        for part in generated_parts:
            # Check if the part contains text (as opposed to other types like function calls)
            if "text" in part:
                print(part["text"])
                print("-" * 50)  # Optional: Add a separator for clarity between different text parts
    else:
        print("No text generated in the response.")

    # Check if grounding metadata is present in the response
    # Grounding metadata provides details about the sources used for generation.
    if "groundingMetadata" in response_data["candidates"][0]:
        grounding_metadata: Dict[str, Any] = response_data["candidates"][0]["groundingMetadata"]

        print("\n--- Google Grounding Info ---")

        # Print search queries used for grounding
        if "webSearchQueries" in grounding_metadata:
            print("Search Queries:")
            for query in grounding_metadata["webSearchQueries"]:
                print(f"  - {query}")

        # Print source links (URIs) from grounding chunks
        if "groundingChunks" in grounding_metadata:
            print("\nSource Links:")
            for chunk in grounding_metadata["groundingChunks"]:
                if "web" in chunk and "uri" in chunk["web"]:
                    print(f"  - {chunk['web']['uri']}")

        print("-" * 50)


def main():
    """
    Main function to orchestrate the Gemini API interaction example.

    This function performs the following steps:
    1. Retrieves the API key from environment variables.
    2. Fetches and displays Gemini model metadata.
    3. Prompts the user to continue.
    4. Defines the text prompt, system instruction, and generation configuration.
    5. Displays a summary of the request parameters.
    6. Calls the Gemini API to generate content.
    7. Displays the generated content and any grounding information.
    """
    api_key: Optional[str] = os.getenv("GEMINI_API_KEY")
    if not api_key:
        print("Error: GEMINI_API_KEY environment variable not set.")
        print("Please set it before running the script (e.g., export GEMINI_API_KEY='YOUR_API_KEY' on Linux/macOS,")
        print("or set GEMINI_API_KEY=YOUR_API_KEY on Windows).")
        return

    # --- Getting the Model Data section ---
    model_data: Optional[Dict[str, Any]] = get_model_metadata(api_key)

    if model_data:
        print("\n--- Default Model Data Before Any Changes ---")
        for key, value in model_data.items():
            print(f"{key}: {value}")
    else:
        print("Could not retrieve model data. Exiting.")
        return

    # Pause execution until user input
    input("\nPress any key to continue...\n")

    # --- Requesting a response section ---
    text_prompt: str = """
Explain the concept of LLM embeddings in simple terms.
"""

    system_instruction: str = """
You are a helpful AI assistant with a flourishing mindset.
"""

    # Generation configuration parameters grouped in a dictionary
    generation_config: Dict[str, Any] = {
        "temperature": 1,
        "topP": 0.95,
        "topK": 64,
        "thinkingConfig": {
            "thinkingBudget": -1,  # Set to -1 for no budget, or a positive integer for a specific budget in milliseconds
            "includeThoughts": False  # Set to True to get the model's internal thinking process
        }
    }

    enable_grounding: bool = False  # Set to True to enable Google Search grounding for the response

    # Display a summary of the request parameters before sending the request
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

    # Generate content using the Gemini API
    response_data: Optional[Dict[str, Any]] = generate_gemini_content(
        api_key=api_key,
        text_prompt=text_prompt,
        system_instruction=system_instruction,
        generation_config=generation_config,
        enable_grounding=enable_grounding
    )

    # Display the response if it was successfully retrieved
    if response_data:
        display_api_response(response_data)
    else:
        print("\nFailed to get a valid response from the Gemini API. Please check the error messages above.")


if __name__ == "__main__":
    # This ensures that main() is called only when the script is executed directly,
    # not when it's imported as a module into another script.
    main()