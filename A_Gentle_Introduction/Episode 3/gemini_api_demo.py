import requests

# Define global variables
GEMINI_API_KEY = "YOUR_GEMINI_API_KEY_HERE"  # Replace with your actual Gemini API key
MODEL = "gemini-2.0-flash-exp"  # Specifies the Gemini model to use

# Construct the API URL
# This URL targets the generateContent endpoint of the specified Gemini model.
# When you place an "f" before a string, Python interprets any expressions within curly braces {} as code to be evaluated.
url = f"https://generativelanguage.googleapis.com/v1beta/models/{MODEL}:generateContent?key={GEMINI_API_KEY}"

# Define the request headers
# Specifies that the request body is in JSON format.
headers = {
    "Content-Type": "application/json"
}

# Define the request data (payload)
# This data contains the prompt for the Gemini model.
data = {
    "contents": [
        {
            "parts": [{"text": "Explain how AI works"}]  # The actual prompt to the model.
        }
    ]
}

# Send the POST request to the Gemini API
# This line sends the constructed request to the specified URL, including the headers and data.
response = requests.post(url, headers=headers, json=data)

# Print the JSON response from the API
# This line prints the entire JSON response received from the Gemini API.
# The response will contain the generated content from the model.
print(response.json())