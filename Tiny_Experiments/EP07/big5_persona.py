import os
from google import genai
from google.genai import types
import textwrap
import datetime

# --- Configuration ---
# Configure Google Gemini API.
# It's highly recommended to set your API key as an environment variable.
# The `genai.Client()` typically looks for `GEMINI_API_KEY` by default.
# If `GEMINI_API_KEY` is not set, it will fall back to `GOOGLE_API_KEY`.
# If you must hardcode it (less secure for production), uncomment the line below and
# replace 'YOUR_GEMINI_API_KEY' with your actual key.

# Attempt to get API key from environment variables
api_key = os.environ.get("GEMINI_API_KEY")
if not api_key:
    api_key = os.environ.get("GOOGLE_API_KEY")

if not api_key:
    print("Error: Neither GEMINI_API_KEY nor GOOGLE_API_KEY environment variable is set.")
    print("Please set one of these environment variables, or uncomment and hardcode your API key in the script (not recommended for production).")
    exit()

# Initialize the Gemini Client. This client will automatically use the API key
# found in the environment variables (GEMINI_API_KEY or GOOGLE_API_KEY, if configured).
client = genai.Client(api_key=api_key) # Pass the explicitly found key for clarity and robustness


# Directory where experiment results will be saved.
OUTPUT_DIR = "experiment_results"
# Ensure the output directory exists.
os.makedirs(OUTPUT_DIR, exist_ok=True)

# --- Trait Level Definitions ---
# Defines the descriptive phrases for each Big Five trait at different intensity levels
# (low, mid, high). These descriptions are used to construct the AI's persona prompt.
TRAIT_DESCRIPTIONS = {
    "Openness to Experience": {
        "low": "You prefer routine, familiarity, and established methods. You are pragmatic and grounded, less inclined towards abstract or unconventional ideas.",
        "mid": "You are open to new experiences when appropriate, but also appreciate tradition. You balance curiosity with practicality.",
        "high": "You are highly imaginative, curious, and intellectually adventurous. You embrace novelty, complexity, and unconventional ideas."
    },
    "Conscientiousness": {
        "low": "You tend to be spontaneous and flexible, sometimes disorganized or impulsive. You may prefer less structure and focus less on meticulous planning.",
        "mid": "You are generally organized and responsible, but can also be adaptable. You manage tasks adequately without extreme perfectionism.",
        "high": "You are highly organized, disciplined, and goal-oriented. You are meticulous, reliable, and committed to achieving tasks efficiently and precisely."
    },
    "Extraversion": {
        "low": "You are reserved, prefer solitude, and find social interactions draining. You are quiet and reflective.",
        "mid": "You are moderately outgoing, enjoying social interaction but also valuing alone time. You can adapt to various social settings.",
        "high": "You are highly sociable, outgoing, and energetic. You enjoy being the center of attention, seek out social stimulation, and are assertive."
    },
    "Agreeableness": {
        "low": "You are skeptical, competitive, and prioritize your own interests. You can be direct and challenging, not always prioritizing others' feelings.",
        "mid": "You are generally cooperative and considerate, but can stand your ground when necessary. You balance empathy with self-interest.",
        "high": "You are highly empathetic, cooperative, and compassionate. You value harmony, trust others, and are eager to help."
    },
    "Neuroticism": { # Note: Low Neuroticism means High Emotional Stability.
        "low": "You are emotionally stable, calm, and resilient. You rarely experience negative emotions like anxiety, anger, or sadness.",
        "mid": "You experience a moderate range of emotions, including some stress or mood fluctuations, but generally cope well.",
        "high": "You are prone to experiencing negative emotions like anxiety, worry, mood swings, and vulnerability to stress. You may react strongly to setbacks."
    }
}

# --- Gemini Model Selection ---
# Defines the available Google Gemini models for selection.
GEMINI_MODELS = {
    "1": {
        "name": "Gemini 1.5 Flash",
        "model_id": "gemini-1.5-flash",
    },
    "2": {
        "name": "Gemini 1.5 Flash 8B",
        "model_id": "gemini-1.5-flash-8b",
    },
    "3": {
        "name": "Gemini 2.0 Flash",
        "model_id": "gemini-2.0-flash",
    },
    "4": {
        "name": "Gemini 2.0 Flash Lite",
        "model_id": "gemini-2.0-flash-lite",
    },
    "5": {
        "name": "Gemini 2.5 Flash",
        "model_id": "gemini-2.5-flash",
    },
    "6": {
        "name": "Gemini 2.5 Flash Lite Preview 06-17",
        "model_id": "gemini-2.5-flash-lite-preview-06-17",
    },
}


# --- Predefined Batch Experiments ---
# A list of predefined experiment configurations. Each dictionary represents a single
# experiment with a specific persona (defined by trait levels), a Gemini model,
# and a user prompt. Set 'trait_levels' to None for a neutral AI persona.
BATCH_EXPERIMENTS = [
    {
        "name": "High Achiever - Project Plan",
        "model_key": "1", # Reference by key from GEMINI_MODELS - this will be overridden by batch and single selection
        "trait_levels": {
            "Openness to Experience": "mid", "Conscientiousness": "high",
            "Extraversion": "mid", "Agreeableness": "mid", "Neuroticism": "low"
        },
        "prompt": "Outline a detailed, step-by-step plan to launch a new software product in 3 months. Include key milestones and potential risks."
    },
    {
        "name": "Supportive Collaborator - Team Coaching",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "mid", "Conscientiousness": "mid",
            "Extraversion": "mid", "Agreeableness": "high", "Neuroticism": "low"
        },
        "prompt": "A junior team member is feeling overwhelmed and insecure about their performance. How would you coach and support them?"
    },
    {
        "name": "Dynamic Innovator - Traffic Solutions",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "high", "Conscientiousness": "high",
            "Extraversion": "mid", "Agreeableness": "mid", "Neuroticism": "low"
        },
        "prompt": "Brainstorm 5 truly unconventional solutions for reducing urban traffic congestion, considering technology, policy, and human behavior."
    },
    {
        "name": "Low Conscientious & High Neurotic - Deadline Panic",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "mid", "Conscientiousness": "low",
            "Extraversion": "mid", "Agreeableness": "mid", "Neuroticism": "high"
        },
        "prompt": "You have a major deadline tomorrow that you haven't started. Describe your feelings and what you would do."
    },
    {
        "name": "Neutral AI Baseline - Project Plan",
        "model_key": "1",
        "trait_levels": None,
        "prompt": "Outline a detailed, step-by-step plan to launch a new software product in 3 months. Include key milestones and potential risks."
    },
    {
        "name": "Energetic Networker - Conference Plan",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "mid", "Conscientiousness": "mid",
            "Extraversion": "high", "Agreeableness": "mid", "Neuroticism": "low"
        },
        "prompt": "You're attending a large industry conference. How would you maximize your networking opportunities and make new connections?"
    },
    {
        "name": "Rule-Bound Traditionalist - Policy Implementation",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "low", "Conscientiousness": "high",
            "Extraversion": "mid", "Agreeableness": "mid", "Neuroticism": "low"
        },
        "prompt": "A new company policy has been issued, which is complex but critical. How would you ensure its correct and complete implementation by your team?"
    },
    {
        "name": "Empathetic Creative - Team Brainstorming",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "high", "Conscientiousness": "mid",
            "Extraversion": "mid", "Agreeableness": "high", "Neuroticism": "low"
        },
        "prompt": "Facilitate a brainstorming session for a new marketing campaign. Encourage diverse ideas and ensure everyone feels heard and valued."
    },
    {
        "name": "Balanced Generalist - Everyday Task",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "mid", "Conscientiousness": "mid",
            "Extraversion": "mid", "Agreeableness": "mid", "Neuroticism": "mid"
        },
        "prompt": "You need to plan a small team lunch. What steps would you take, considering diverse preferences?"
    },
    {
        "name": "Conflict-Averse & Anxious - Difficult Conversation",
        "model_key": "1",
        "trait_levels": {
            "Openness to Experience": "mid", "Conscientiousness": "mid",
            "Extraversion": "mid", "Agreeableness": "high", "Neuroticism": "high"
        },
        "prompt": "You need to tell a close friend that their recent behavior is causing problems, but you're worried about hurting their feelings and causing an argument. How would you approach this?"
    }
]

# --- Core LLM Interaction and Persona Generation Functions ---

# The 'client' object is now defined globally after API key setup.
def get_gemini_response(persona_description: str, user_prompt: str, model_id: str) -> str:
    """
    Obtains a response from the Google Gemini API using the global client.

    Args:
        persona_description (str): The system prompt defining the AI's persona.
        user_prompt (str): The user's query to the AI.
        model_id (str): The identifier for the Gemini model to use (e.g., "gemini-1.5-flash").

    Returns:
        str: The AI's generated response, or an error message if the API call fails.
    """
    try:
        # Pass system_instruction directly to generate_content via config
        response = client.models.generate_content(
            model=model_id,
            config=types.GenerateContentConfig(
                system_instruction=persona_description),
            contents=user_prompt           
        )
        return response.text
    except Exception as e:
        return f"Error with Gemini API: {e}"

def get_trait_level(trait_name: str) -> str:
    """
    Prompts the user to select an intensity level (low/mid/high) for a given Big Five trait.

    Args:
        trait_name (str): The name of the personality trait (e.g., "Openness to Experience").

    Returns:
        str: The selected level ('low', 'mid', or 'high').
    """
    while True:
        level = input(f"Set level for {trait_name} (low/mid/high): ").strip().lower()
        if level in ["low", "mid", "high"]:
            return level
        print("Invalid level. Please enter 'low', 'mid', or 'high'.")

def generate_dynamic_persona_description(trait_levels: dict) -> str:
    """
    Constructs a comprehensive persona description string based on a dictionary
    of selected trait levels. This string is used as the system prompt for the AI.

    Args:
        trait_levels (dict): A dictionary where keys are trait names and values are
                             their selected levels ('low', 'mid', 'high').

    Returns:
        str: The full persona description.
    """
    description_parts = ["You are an AI assistant embodying the following personality traits:"]
    for trait, level in trait_levels.items():
        # Appends the specific description for each trait's chosen level.
        description_parts.append(f"- {trait}: {TRAIT_DESCRIPTIONS[trait][level]}")
    description_parts.append("\nYour responses should reflect these personality characteristics consistently.")
    return "\n".join(description_parts)

# --- Experiment Execution and Reporting Functions ---

def save_experiment_result(exp_info: dict, response: str, filename: str):
    """
    Appends a single experiment's result to a specified Markdown log file.

    Args:
        exp_info (dict): A dictionary containing details about the experiment,
                         such as name, model ID, trait summary, full persona description,
                         and the user prompt.
        response (str): The AI's generated response.
        filename (str): The full path to the Markdown file where results will be saved.
    """
    # Use 'a' mode for appending to the file.
    with open(filename, "a", encoding="utf-8") as f_out:
        f_out.write(f"## {exp_info['name']} - {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f_out.write(f"**Model:** `{exp_info['model_name']} ({exp_info['model_id']})`\n")

        trait_summary = exp_info.get('trait_summary', 'N/A')
        f_out.write(f"**Persona Traits:** {trait_summary}\n")

        # Include the full persona system prompt if available (for custom/dynamic personas).
        if exp_info.get('full_persona_description'):
            f_out.write(f"**Full Persona System Prompt:**\n```\n{exp_info['full_persona_description']}\n```\n")

        f_out.write(f"**User Prompt:**\n```\n{exp_info['user_prompt']}\n```\n")
        f_out.write(f"**AI Response:**\n```\n{response}\n```\n\n---\n\n")
    print(f"Result saved to {filename}")

def process_single_experiment(exp_name: str, model_info: dict, trait_levels: dict | None, user_prompt: str, is_batch_run: bool = False):
    """
    Executes a single experiment by generating a persona, calling the AI,
    displaying the response, and saving it to the log file.

    Args:
        exp_name (str): A descriptive name for the experiment.
        model_info (dict): A dictionary containing 'name' and 'model_id' for the Gemini model.
        trait_levels (dict | None): A dictionary of trait levels for the persona,
                                    or None for a neutral persona.
        user_prompt (str): The prompt to send to the AI.
        is_batch_run (bool): True if this is part of a larger batch run (affects console pausing).
    """
    persona_description = ""
    trait_summary = "N/A"

    if trait_levels:
        persona_description = generate_dynamic_persona_description(trait_levels)
        # Create a concise summary of traits for display and logging.
        trait_summary = ", ".join([f"{t.split(' to Experience')[0]}: {l.capitalize()}" for t, l in trait_levels.items()])
    else:
        persona_description = "You are a helpful AI assistant. Respond as a neutral, general-purpose AI."
        trait_summary = "No Specific Persona (Neutral Baseline)"

    print(f"\n--- Generating response for: {exp_name} (Model: {model_info['name']}) ---")
    print(f"  Persona: {trait_summary}")
    print(f"  Prompt: {textwrap.shorten(user_prompt, width=70, placeholder='...')}")
    print("Please wait...\n")

    response = get_gemini_response(persona_description, user_prompt, model_info['model_id'])

    # Display the AI's response on the console.
    print("\n" + "="*50)
    print("AI Response:")
    print(response)
    print("=" * 50 + "\n")

    # Prepare experiment info for logging.
    exp_info = {
        "name": exp_name,
        "model_name": model_info['name'],
        "model_id": model_info['model_id'],
        "trait_summary": trait_summary,
        "full_persona_description": persona_description,
        "user_prompt": user_prompt
    }
    # Determine the filename based on whether it's a batch run or interactive.
    log_filename = os.path.join(OUTPUT_DIR, "gemini_batch_experiments_log.md") if is_batch_run else os.path.join(OUTPUT_DIR, "gemini_interactive_experiments_log.md")
    save_experiment_result(exp_info, response, log_filename)

    if not is_batch_run: # Only pause for interactive runs to allow user to read.
        input("Press Enter to continue...")

# --- Menu Display Functions ---

def display_main_menu():
    """
    Displays the main menu options for the Gemini Persona Experimenter.
    """
    print("\n" + "="*50)
    print("      Dynamic Gemini Persona Experimenter      ")
    print("="*50)
    print("\nSelect an option:")
    print("  1. Create Custom Persona & Get Interactive AI Response")
    print("  2. Use Neutral Persona & Get Interactive AI Response")
    print(f"  3. Run ALL {len(BATCH_EXPERIMENTS)} Predefined Batch Experiments (Select model once)")
    print("  4. Select and Run a Single Predefined Experiment")

    print("\nOther Options:")
    print("  'q' or 'quit' to exit")
    print("="*50)

def display_model_selection_menu() -> dict | None:
    """
    Displays the menu for selecting a Gemini model.
    Returns the selected model's info (dict with 'name' and 'model_id') or None if invalid.
    """
    print("\n--- Select a Gemini Model ---")
    for key, model_info in GEMINI_MODELS.items():
        print(f"  {key}. {model_info['name']}")
    print("----------------------------")
    
    while True:
        model_choice_key = input("Enter model number: ").strip()
        if model_choice_key in GEMINI_MODELS:
            return GEMINI_MODELS[model_choice_key]
        else:
            print("Invalid model selection. Please enter a valid number.")
            # Keep looping until valid input

def display_batch_experiment_menu():
    """
    Displays the sub-menu listing all predefined batch experiments, allowing
    the user to select a single one to run.
    """
    print("\n--- Select a Predefined Experiment ---")
    for i, exp in enumerate(BATCH_EXPERIMENTS):
        # Generate a brief summary of the persona for display.
        summary = ", ".join([f"{t.split(' to Experience')[0]}: {l.capitalize()}" for t, l in exp['trait_levels'].items()]) if exp['trait_levels'] else "Neutral"
        print(f"  {i+1}. {exp['name']} (Persona: {summary})")
    print("  'b' to go back to main menu")
    print("--------------------------------------")

# --- Main Application Logic ---

def main():
    """
    The main function that drives the interactive menu and experiment execution.
    """
    while True:
        display_main_menu()

        main_choice = input("Enter your choice (1, 2, 3, 4, or q): ").strip().lower()
        if main_choice in ['q', 'quit']:
            print("Exiting the Gemini Persona Experimenter. Goodbye!")
            break

        if main_choice == '3':
            # Option to run all predefined batch experiments.
            print(f"\n--- Running ALL {len(BATCH_EXPERIMENTS)} predefined experiments. ---")
            
            # --- NEW: Select model for the entire batch run ---
            selected_model_info_for_batch = display_model_selection_menu()
            if selected_model_info_for_batch is None:
                # If user enters invalid model, go back to main menu
                continue 
            # --- END NEW ---

            for i, experiment in enumerate(BATCH_EXPERIMENTS):
                # Now, use the selected_model_info_for_batch for every experiment
                process_single_experiment(
                    exp_name=experiment["name"],
                    model_info=selected_model_info_for_batch, # Use the model selected for the entire batch
                    trait_levels=experiment["trait_levels"],
                    user_prompt=experiment["prompt"],
                    is_batch_run=True # Mark as a batch run for specific logging filename.
                )
            print(f"\nAll batch experiments finished. Check '{OUTPUT_DIR}/gemini_batch_experiments_log.md'")
            continue # Return to the main menu.

        elif main_choice == '4':
            # Option to select and run a single predefined experiment.
            while True:
                display_batch_experiment_menu()
                batch_exp_choice = input("Enter experiment number or 'b' to go back: ").strip().lower()

                if batch_exp_choice == 'b':
                    break # Go back to main menu.

                try:
                    exp_index = int(batch_exp_choice) - 1
                    if 0 <= exp_index < len(BATCH_EXPERIMENTS):
                        selected_experiment = BATCH_EXPERIMENTS[exp_index]
                        
                        # Get model selection for this specific run.
                        selected_model_info = display_model_selection_menu()
                        if selected_model_info is None: # User entered invalid model choice, display_model_selection_menu already printed error
                            continue # Stay in batch experiment selection loop to allow re-selection

                        process_single_experiment(
                            exp_name=selected_experiment["name"],
                            model_info=selected_model_info, # Use selected model info
                            trait_levels=selected_experiment["trait_levels"],
                            user_prompt=selected_experiment["prompt"],
                            is_batch_run=False # This is an interactive run of a predefined experiment.
                        )
                        # After processing, stay in the batch experiment selection loop. Uncomment 'break' below
                        # to return to main menu after a single run.
                        # break 
                    else:
                        print("Invalid experiment number. Please try again.")
                except ValueError:
                    print("Invalid input. Please enter a number or 'b'.")
            continue # Return to the main menu after exiting the single experiment selection loop.

        # Handle interactive custom persona (Option 1) or neutral persona (Option 2) creation.
        user_prompt_for_interactive = "" # Initialize here for scope
        selected_trait_levels_for_interactive_process = None # Initialize as None for clarity

        if main_choice == '1':
            print("\n--- Custom Persona Creation ---")
            selected_trait_levels = {}
            for trait in TRAIT_DESCRIPTIONS.keys():
                selected_trait_levels[trait] = get_trait_level(trait)
            selected_trait_levels_for_interactive_process = selected_trait_levels

            persona_description_for_interactive_display = generate_dynamic_persona_description(selected_trait_levels)
            print("\n--- Generated Persona Description ---")
            print(persona_description_for_interactive_display)
            print("------------------------------------\n")

        elif main_choice == '2':
            print("\n--- Using Neutral Persona ---")
            # For neutral persona, selected_trait_levels_for_interactive_process remains None
        else:
            print("Invalid selection. Please try again.")
            continue # Loop back to main menu for invalid initial choice.

        # Model selection for interactive runs (options 1 and 2).
        selected_model_info_for_interactive = display_model_selection_menu()
        if selected_model_info_for_interactive is None: # User entered invalid model choice
            continue # Loop back to main menu

        user_prompt_for_interactive = input("\nEnter your prompt for the AI: \n> ").strip()
        if not user_prompt_for_interactive:
            print("Prompt cannot be empty. Please try again.")
            continue # Loop back to main menu.

        # Process and save the interactive run using the common function.
        process_single_experiment(
            exp_name=f"Interactive Run - {selected_model_info_for_interactive['name']}",
            model_info=selected_model_info_for_interactive,
            trait_levels=selected_trait_levels_for_interactive_process, # Pass actual trait levels or None
            user_prompt=user_prompt_for_interactive,
            is_batch_run=False # This is always an interactive run.
        )

if __name__ == "__main__":
    main()