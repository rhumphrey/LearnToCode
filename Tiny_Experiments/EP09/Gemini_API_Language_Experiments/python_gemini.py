import requests
import os

# It is highly recommended to set your API key as an environment variable
# Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
# Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
API_KEY = os.getenv("GEMINI_API_KEY")

GEMINI_API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
GEMINI_MODEL_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash"


# Getting the Model Data section

params = {
    "key": API_KEY
}

response = requests.get(GEMINI_MODEL_ENDPOINT, params=params)

model_data = response.json()

# Print the model metadata
print("\n--- Default Model Data Before Any Changes ---")
for key, value in model_data.items():
    print(f"{key}: {value}")


input("\nPress any key to continue\n")


# Requesting a response section

text = """
Explain the concept of LLM embeddings in simple terms.
"""

instruction = """
You are a helpful AI assistant with a flourishing mindset

"""

temperature = 1
topP = 0.95
topK = 64

thinking_budget = -1
include_thoughts = False

enable_grounding = False

headers = {
    "x-goog-api-key": API_KEY,
    "Content-Type": "application/json"
}

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
    "tools": [
      {
        "google_search": {}
      }
    ]
}


if enable_grounding:
    if "tools" not in request_payload:
      request_payload["tools"] = [
      {
        "google_search": {}
      }
    ]
else:
    if "tools" in request_payload:
        del request_payload["tools"]


# Send the POST request
print("Sending Request...")
response = requests.post(GEMINI_API_ENDPOINT, headers=headers, json=request_payload)

# Parse the JSON response body into a Python dictionary
response_data = response.json()

# Extract the generated text from the structured response
generated_parts = response_data["candidates"][0]["content"]["parts"]

print("\n--- Response Data ---")

# Deal with potentially multiple parts of text (affected by 'include thoughts' setting)
print("Generated Text:\n")
for part in generated_parts:
    if "text" in part:
        print(part["text"])
        print("-" * 50) # Optional: Add a separator for clarity between different text parts



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


print("\nSummary of values set for this request")
print("---------------------------------------")
print(f"text: {text.strip()}")
print(f"instruction: {instruction.strip()}")
print(f"temperature: {temperature}")
print(f"topP: {topP}")
print(f"topK: {topK}")
print(f"thinking_budget: {thinking_budget}")
print(f"include_thoughts: {include_thoughts}")
print(f"enable_grounding: {enable_grounding}")
print("---------------------------------------")
