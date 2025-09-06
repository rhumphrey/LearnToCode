import os
import json
import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Dict, Any

import requests

# --- Configuration ---

# Set up basic logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# It is highly recommended to set your API key as an environment variable
API_KEY = os.getenv("GEMINI_API_KEY")
if not API_KEY:
    raise ValueError("GEMINI_API_KEY environment variable not set.")

API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
OUTPUT_PATH = Path("rascef_experiments.md")

# --- Data Structures ---

@dataclass
class Experiment:
    """A dataclass to represent a single prompting experiment."""
    name: str
    focus: str
    text: str
    instruction: str = "" # Default to an empty string for simplicity

# --- Experiment Definitions ---

EXPERIMENTS: List[Experiment] = [
    Experiment(
        name="Experiment 1: Baseline (Just Action)",
        focus="Action (implied)",
        text="Explain what a CPU is."
    ),
    Experiment(
        name="Experiment 2: Adding a Role (R)",
        focus="Role",
        instruction="You are a friendly expert explaining a concept to a complete beginner. Be patient and use simple, non-technical analogies.",
        text="Explain what a CPU is."
    ),
    Experiment(
        name="Experiment 3: Adding Context (C)",
        focus="Context",
        text="Explain what a CPU is. I am writing a purchase guide for gamers and need to explain why a good CPU is important for playing modern video games."
    ),
    Experiment(
        name="Experiment 4: Defining the Format (F)",
        focus="Format",
        text="Explain what a CPU is. Provide the output as a JSON object with three keys: \"fullName\", \"primaryFunction\", and \"keyMetrics\" (which should be an array of strings)."
    ),
    Experiment(
        name="Experiment 5: Providing an Example (E)",
        focus="Example",
        text="Provide a one-sentence, poetic definition for a technology concept.\nExample: \"The Internet: A silent, invisible web of light and whispers, connecting all minds as one.\"\nNow, do the same for a CPU."
    ),
    Experiment(
        name="Experiment 6: Outlining Steps (S)",
        focus="Steps",
        text="Explain what a CPU is by following these exact steps:\n1. Start with the full name: \"Central Processing Unit\".\n2. Provide a one-sentence analogy.\n3. List its three main tasks in a bulleted list."
    ),
    Experiment(
        name="Experiment 7: Combining Role + Action + Format (R+A+F)",
        focus="Action, Role, Format",
        instruction="You are a project manager creating a task ticket.",
        text="Describe the task of \"learning what a CPU is\". Format it as a markdown checklist."
    ),
    Experiment(
        name="Experiment 8: Combining Action + Context + Example (A+C+E)",
        focus="Action, Context, Example",
        text="Generate three different advertising slogans for a new, ultra-fast CPU. The target audience is creative professionals like video editors and 3D artists.\nExample Style: \"Think Faster. Create Faster.\"\nNow, generate three more in a similar style."
    ),
    Experiment(
        name="Experiment 9: Combining Role + Action + Steps + Context (R+A+S+C)",
        focus="Role, Action, Steps, Context",
        instruction="You are a helpful tutor creating a study guide.",
        text="Create a study guide entry for the CPU. The student is a visual learner and finds text boring.\n1. Create a title for the study guide section.\n2. Use a simple emoji to represent the CPU.\n3. Explain its function in one short sentence."
    ),
    Experiment(
        name="Experiment 10: Full RASCEF",
        focus="Role, Action, Steps, Context, Example, Format",
        instruction="You are a scriptwriter for a popular tech YouTube channel that makes complex topics simple and fun.",
        text="Write a 30-second video script intro about the CPU. The goal is to hook the viewer immediately. \n\nFollow this structure: \n1. Start with a relatable question.\n2. Briefly state the CPU's nickname.\n3. End with a promise of what the viewer will learn.\n\nExample of a line you might write: \"It's the brain of your computer, the master chef in the kitchen...\" \n\nThe final output must be in plain text, with no special markdown."
    )
]

# --- Core Logic ---

def call_gemini_api(session: requests.Session, experiment: Experiment) -> Dict[str, Any]:
    """Sends a request to the Gemini API for a given experiment."""
    request_payload = {
        "system_instruction": {"parts": [{"text": experiment.instruction}]},
        "contents": [{"parts": [{"text": experiment.text}]}],
        "generationConfig": {"temperature": 0.7, "topP": 0.95, "topK": 64},
        "tools": [{"google_search": {}}]
    }
    response = session.post(API_ENDPOINT, json=request_payload)
    response.raise_for_status()  # Raise HTTPError for bad responses (4xx or 5xx)
    return response.json()

def write_experiment_to_file(file_handle, experiment: Experiment, response_data: Dict[str, Any] = None, error: Exception = None):
    """Formats and writes the result of a single experiment to the file."""
    file_handle.write(f"---\n\n## {experiment.name}\n\n")
    file_handle.write(f"**RASCEF Focus:** {experiment.focus}\n\n")
    
    file_handle.write("### Prompt Details\n\n")
    file_handle.write("#### System Instruction (Role)\n")
    file_handle.write(f"```\n{experiment.instruction or 'N/A'}\n```\n\n")
    file_handle.write("#### User Text (Prompt)\n")
    file_handle.write(f"```\n{experiment.text}\n```\n\n")

    if error:
        file_handle.write("### Error\n\n")
        file_handle.write(f"```\nAPI request failed: {error}\n```\n\n")
        return

    file_handle.write("### Gemini 2.5 Flash Response\n\n")
    
    # Safely extract generated text
    try:
        candidates = response_data.get("candidates", [])
        if candidates:
            generated_parts = candidates[0].get("content", {}).get("parts", [])
            for part in generated_parts:
                file_handle.write(f"{part.get('text', '')}\n")
        else:
             file_handle.write("No content generated or error in response format.\n")
             file_handle.write(f"```json\n{json.dumps(response_data, indent=2)}\n```\n")
    except (IndexError, KeyError) as e:
        file_handle.write(f"Error parsing response content: {e}\n")
        file_handle.write(f"```json\n{json.dumps(response_data, indent=2)}\n```\n")
    
    # Safely extract web search queries
    try:
        grounding_metadata = response_data.get("candidates", [{}])[0].get("groundingMetadata", {})
        search_queries = grounding_metadata.get("webSearchQueries", [])
    except IndexError:
        search_queries = []

    file_handle.write("\n#### Web Search Queries Considered\n")
    if search_queries:
        for query in search_queries:
            file_handle.write(f"- `{query}`\n")
    else:
        file_handle.write("None\n")
        
    file_handle.write("\n\n")


def main():
    """Runs all defined experiments and saves the results to a markdown file."""
    headers = {
        "x-goog-api-key": API_KEY,
        "Content-Type": "application/json"
    }
    
    # Check if the output file already exists to avoid writing the header multiple times.
    file_exists = OUTPUT_PATH.exists()

    # Use a session object for connection pooling and efficiency
    with requests.Session() as session:
        session.headers.update(headers)
        
        # Open the output file in append mode ('a')
        with OUTPUT_PATH.open("a", encoding="utf-8") as f:
            # If the file is new, write the main header.
            if not file_exists:
                f.write("# Gemini 2.5 Flash RASCEF Experiments\n\n")
                f.write("This document contains the results of prompting experiments designed to explore the RASCEF framework.\n\n")
                logging.info(f"Creating new output file: '{OUTPUT_PATH}'")

            for exp in EXPERIMENTS:
                logging.info(f"Running {exp.name}...")
                try:
                    api_response = call_gemini_api(session, exp)
                    write_experiment_to_file(f, exp, response_data=api_response)
                except requests.exceptions.RequestException as e:
                    logging.error(f"Failed {exp.name}: {e}")
                    write_experiment_to_file(f, exp, error=e)
                logging.info(f"Finished {exp.name}.")
            
            f.write("\n\n")

    logging.info(f"All experiments complete. Results have been appended to '{OUTPUT_PATH}'")

if __name__ == "__main__":
    main()