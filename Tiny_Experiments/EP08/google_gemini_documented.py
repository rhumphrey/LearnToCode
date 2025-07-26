import requests
import os

# ==============================================================================
# API Configuration and Constants
# ==============================================================================

# It is highly recommended to set your API key as an environment variable
# This practice keeps sensitive information out of your code.
# Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
# Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
API_KEY = os.getenv("GEMINI_API_KEY")

# The API endpoint for generating content using the specified Gemini model.
GEMINI_API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"

# The API endpoint for retrieving metadata about the specified Gemini model.
GEMINI_MODEL_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash"

# ==============================================================================
# Getting the Model Data
# ==============================================================================

print("--- Fetching Model Metadata ---")

# Define the parameters for the GET request.
# The 'key' parameter is required for authentication.
params = {
    "key": API_KEY
}

# Send a GET request to the model metadata endpoint.
response = requests.get(GEMINI_MODEL_ENDPOINT, params=params)

# Parse the JSON response body into a Python dictionary.
model_data = response.json()

# Print the model metadata in a readable format.
print("\n--- Default Model Data Before Any Changes ---")
for key, value in model_data.items():
    print(f"{key}: {value}")

# Pause the script to allow the user to review the metadata before continuing.
input("\nPress any key to continue to content generation\n")


# ==============================================================================
# Content Generation Request Setup
# ==============================================================================

print("--- Preparing Content Generation Request ---")

# Define the user's prompt or the text for the model to process.
text = """
Explain the concept of LLM embeddings in simple terms.
"""

# Define a system instruction to set the model's persona or behavior.
instruction = """
You are a helpful AI assistant with a flourishing mindset.

"""

# Define generation configuration parameters.
# Temperature controls the randomness of the output. Higher values lead to more creative responses.
temperature = 1
# topP (nucleus sampling) controls the diversity of the output.
topP = 0.95
# topK limits the sampling to the top k most likely tokens.
topK = 64

# Configuration for the model's "thinking" process (used for advanced features like Chain of Thought).
# A thinking budget of -1 means no limit.
thinking_budget = -1
# A boolean to determine if the model's internal thoughts should be included in the response.
include_thoughts = False

# A boolean to control whether to use Google Search for grounding the response.
enable_grounding = False

# Define the headers for the API request.
# The 'x-goog-api-key' header is used for authentication.
# The 'Content-Type' header specifies that the request body is JSON.
headers = {
    "x-goog-api-key": API_KEY,
    "Content-Type": "application/json"
}

# Construct the main payload for the API request.
request_payload = {
    "system_instruction": {
      "parts": [
        {
          "text": instruction
        }
      ]
    },
    "contents": [
        {
            "parts": [
                {"text": text }
            ]
        }
    ],
    "generationConfig": {
      "temperature": temperature,
      "topP": topP,
      "topK": topK,
      "thinkingConfig": {
        "thinkingBudget": thinking_budget,
        "includeThoughts": include_thoughts
      }
    },
    # The 'tools' section is where you can specify tools for the model to use,
    # such as Google Search for grounding.
    "tools": [
      {
        "google_search": {}
      }
    ]
}

# Conditionally enable or disable grounding based on the `enable_grounding` flag.
if enable_grounding:
    # If grounding is enabled, ensure the 'Google Search' tool is present.
    # Note: The tool is already in the default payload, but this makes the logic explicit.
    if "tools" not in request_payload:
        request_payload["tools"] = [{"google_search": {}}]
else:
    # If grounding is disabled, remove the 'tools' section from the payload.
    if "tools" in request_payload:
        del request_payload["tools"]

# ==============================================================================
# Sending the Request and Processing the Response
# ==============================================================================

# Send the POST request to the API endpoint with the defined payload.
print("\n--- Sending Request... ---")
response = requests.post(GEMINI_API_ENDPOINT, headers=headers, json=request_payload)

# Parse the JSON response body into a Python dictionary.
response_data = response.json()

# Extract the generated text from the structured response.
# The path to the generated content is `response_data["candidates"][0]["content"]["parts"]`.
generated_parts = response_data["candidates"][0]["content"]["parts"]

print("\n--- Response Data ---")

# Iterate through the parts of the response and print the text content.
# This loop handles cases where the response might contain multiple text parts (e.g., with included thoughts).
print("Generated Text:\n")
for part in generated_parts:
    if "text" in part:
        print(part["text"])
        # Optional: Add a separator for clarity between different text parts.
        print("-" * 50)

# Check if grounding metadata is present in the response.
# This metadata contains information about the sources used for grounding.
if "groundingMetadata" in response_data["candidates"][0]:
    grounding_metadata = response_data["candidates"][0]["groundingMetadata"]
    
    print("\n--- Google Grounding Info ---")
    
    # Print the search queries that the model used.
    if "webSearchQueries" in grounding_metadata:
        print("Search Queries:")
        for query in grounding_metadata["webSearchQueries"]:
            print(f"  - {query}")
            
    # Print the source links (URIs) that the model used for grounding.
    if "groundingChunks" in grounding_metadata:
        print("\nSource Links:")
        for chunk in grounding_metadata["groundingChunks"]:
            if "web" in chunk and "uri" in chunk["web"]:
                print(f"  - {chunk['web']['uri']}")
    
    print("-" * 50)

# ==============================================================================
# Summary and Conclusion
# ==============================================================================

# Print a summary of the configuration values used for the request.
print("\n--- Summary of Request Parameters ---")
print(f"text: {text.strip()}")
print(f"instruction: {instruction.strip()}")
print(f"temperature: {temperature}")
print(f"topP: {topP}")
print(f"topK: {topK}")
print(f"thinking_budget: {thinking_budget}")
print(f"include_thoughts: {include_thoughts}")
print(f"enable_grounding: {enable_grounding}")
print("-" * 50)