import os
import requests
import datetime
import json
import re
from dataclasses import dataclass
from typing import Optional, Tuple

# --- Configuration ---
GEMINI_API_KEY = os.getenv("GEMINI_API_KEY")
GEMINI_MODEL_NAME = "gemini-2.5-flash"
LEARNING_PLANS_FOLDER = "learning_plans"
GEMINI_API_BASE_URL = "https://generativelanguage.googleapis.com/v1beta/models"

@dataclass
class GenerationParameters:
    temperature: float
    top_p: float
    top_k: int

@dataclass
class AppConfig:
    generation_params: GenerationParameters

def create_learning_plan_prompt(language: str) -> str:
    """Creates a prompt for generating a learning plan for a programming language."""
    return (
        f"Create a comprehensive 4-week learning plan for {language} with the following requirements:\n\n"
        "1. Schedule: 3 days per week, 3 hours per day\n"
        "2. Focus on experiential learning (learning by doing)\n"
        "3. Include tiny habit suggestions for each week\n"
        "4. Include a tiny experiment for each week\n"
        "5. Structure the plan with clear weekly goals and daily activities\n\n"
        "Format the response as a Markdown document with the following sections:\n"
        "- Title: Learning Plan for [Language]\n"
        "- Brief overview of the language and its applications\n"
        "- Weekly breakdown (Week 1 to Week 4)\n"
        "  - Weekly goals\n"
        "  - Daily activities for each of the 3 days\n"
        "  - Tiny habit suggestions\n"
        "  - Weekly experiment\n"
        "- Recommended resources\n"
        "- Tips for success\n\n"
        "Make the plan practical, actionable, and suitable for a beginner to intermediate learner."
    )

def extract_llm_response_text(gemini_response: dict) -> str:
    """Extracts the main text content from the Gemini API response."""
    try:
        candidate = gemini_response['candidates'][0]
        return candidate['content']['parts'][0]['text']
    except (KeyError, IndexError):
        return "No relevant content found or response was not in expected format."

def create_api_payload(prompt_text: str, generation_params: GenerationParameters) -> dict:
    """Creates the payload for the Gemini API request."""
    return {
        "contents": [{"parts": [{"text": prompt_text}]}],
        "generationConfig": {
            "temperature": generation_params.temperature,
            "topP": generation_params.top_p,
            "topK": generation_params.top_k,
            "thinkingConfig": {"thinkingBudget": -1}
        },
        "safetySettings": [
            {"category": "HARM_CATEGORY_HARASSMENT", "threshold": "BLOCK_NONE"},
            {"category": "HARM_CATEGORY_HATE_SPEECH", "threshold": "BLOCK_NONE"},
            {"category": "HARM_CATEGORY_SEXUALLY_EXPLICIT", "threshold": "BLOCK_NONE"},
            {"category": "HARM_CATEGORY_DANGEROUS_CONTENT", "threshold": "BLOCK_NONE"},
        ],
        "tools": [{"google_search": {}}],
    }

def make_api_request(prompt_text: str, generation_params: GenerationParameters) -> Tuple[Optional[dict], Optional[str]]:
    """
    Sends the prompt to the Google Gemini API.
    Returns a tuple of (response, error_message)
    """
    if not GEMINI_API_KEY:
        return None, "API key not found. Please set the GEMINI_API_KEY environment variable."

    endpoint = f"{GEMINI_API_BASE_URL}/{GEMINI_MODEL_NAME}:generateContent"
    headers = {"Content-Type": "application/json"}
    payload = create_api_payload(prompt_text, generation_params)

    try:
        response = requests.post(
            endpoint, 
            headers=headers, 
            json=payload, 
            params={"key": GEMINI_API_KEY},
            timeout=300
        )
        response.raise_for_status()
        return response.json(), None
    except requests.exceptions.RequestException as err:
        error_msg = f"API error occurred: {err}"
        try:
            if response is not None and hasattr(response, 'text'):
                error_details = json.loads(response.text)
                if 'error' in error_details and 'message' in error_details['error']:
                    error_msg += f"\nAPI Error Details: {error_details['error']['message']}"
                else:
                    error_msg += f"\nAPI Raw Error Response: {json.dumps(error_details, indent=2)}"
            elif hasattr(response, 'text'):
                error_msg += f"\nAPI Raw Error Response: {response.text}"
        except (json.JSONDecodeError, AttributeError):
            pass
        return None, error_msg

def ensure_directory_exists(folder: str) -> None:
    """Ensures the output directory exists, avoiding race conditions."""
    try:
        os.makedirs(folder, exist_ok=True)
    except OSError as e:
        print(f"Error creating directory {folder}: {e}")

def write_to_file(folder: str, content: str, filename_prefix: str) -> None:
    """Saves content to a timestamped Markdown file."""
    ensure_directory_exists(folder)
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(folder, f"{filename_prefix}_{timestamp}.md")

    try:
        with open(filename, "w", encoding="utf-8") as f:
            f.write(content)
        print(f"Learning plan saved to: {filename}")
    except IOError as e:
        print(f"Error saving file: {e}")

def create_learning_plan(language: str, app_config: AppConfig) -> None:
    """Creates a learning plan for a programming language."""
    if not GEMINI_API_KEY:
        print("Error: GEMINI_API_KEY environment variable is not set.")
        print("Please set it before running the application.")
        return

    # Construct prompt
    print(f"Creating learning plan for {language}...")
    learning_prompt = create_learning_plan_prompt(language)

    # Make API request
    print(f"Sending request to Google Gemini API using {GEMINI_MODEL_NAME}...")
    gemini_response, error_message = make_api_request(learning_prompt, app_config.generation_params)

    if error_message:
        print(f"API request failed: {error_message}")
        return
        
    if not gemini_response:
        print("Failed to get valid response from Gemini API.")
        return

    # Process response
    print("Processing response...")
    learning_plan_content = extract_llm_response_text(gemini_response) 
    
    # Save results
    print("Saving learning plan...")
    write_to_file(LEARNING_PLANS_FOLDER, learning_plan_content, f"{language.lower().replace(' ', '_')}_learning_plan")
    print("Learning plan created successfully!")

def main():
    """Main function for the learning plan creation application."""
    print("Welcome to the Learning Plan Creator!")
    print(f"Using model: {GEMINI_MODEL_NAME}")
    
    # Default generation parameters
    app_config = AppConfig(
        generation_params=GenerationParameters(
            temperature=0.7,
            top_p=0.95,
            top_k=64
        )
    )
    
    # Get language from user
    language = input("Enter the programming language you want to learn: ").strip()
    if language:
        create_learning_plan(language, app_config)
    else:
        print("No language entered. Please try again.")

if __name__ == "__main__":
    main()