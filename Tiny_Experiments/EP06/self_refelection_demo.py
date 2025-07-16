import ollama # Import the Ollama library for interacting with local LLMs.
import google.generativeai as genai # Import the Google Generative AI library for interacting with Gemini models.
import os # Import the os module to access environment variables.
import time # Import the time module for delays, if needed (though not explicitly used for delays in the core logic here, often useful for user experience).

# --- Configuration Variables ---
# Sets the currently active LLM source. Can be "ollama" for local models or "gemini" for cloud-based models.
CURRENT_LLM_SOURCE = "ollama"
# Specifies the default Ollama model to use.
CURRENT_OLLAMA_MODEL = "gemma3"
# Specifies the default Gemini model to use.
CURRENT_GEMINI_MODEL = "gemini-2.5-flash"
# Retrieves the Gemini API key from environment variables. This is crucial for authenticating with the Gemini API.
GEMINI_API_KEY = os.getenv("GEMINI_API_KEY")

# --- API Key Configuration ---
# Checks if the GEMINI_API_KEY environment variable is set.
if GEMINI_API_KEY:
    # If the key is found, configure the genai library with the API key.
    genai.configure(api_key=GEMINI_API_KEY)
else:
    # If the key is not found, print a warning to the console, as Gemini models won't be available.
    print("Warning: GEMINI_API_KEY environment variable not found. Gemini models will not be available.")

# --- Core LLM Interaction Function ---
def get_llm_response(messages, **kwargs):
    """
    Retrieves a response from the currently selected LLM (Ollama or Gemini).

    Args:
        messages (list): A list of message dictionaries in the format
                         [{'role': 'user', 'content': '...'}] for Ollama/Gemini.
        **kwargs: Arbitrary keyword arguments to pass to the LLM API (e.g., options for Ollama).

    Returns:
        str: The content of the LLM's response, or an error message if an issue occurs.
    """
    if CURRENT_LLM_SOURCE == "ollama":
        try:
            # Call the Ollama chat API with the specified model and messages.
            # stream=False ensures a single, complete response.
            # options=kwargs allows passing additional parameters like 'temperature'.
            response = ollama.chat(
                model=CURRENT_OLLAMA_MODEL,
                messages=messages,
                stream=False,
                options=kwargs
            )
            # Extract and return the content from the Ollama response.
            return response['message']['content']
        except ollama.ResponseError as e:
            # Handles errors specific to the Ollama API (e.g., server not running, model not pulled).
            print(f"Ollama Error: {e}")
            print(f"Ensure Ollama server is running and model '{CURRENT_OLLAMA_MODEL}' is pulled.")
            return "ERROR: Could not get Ollama LLM response."
        except Exception as e:
            # Handles any other unexpected errors during Ollama interaction.
            print(f"An unexpected Ollama error occurred: {e}")
            return "ERROR: An unexpected Ollama error occurred."
    elif CURRENT_LLM_SOURCE == "gemini":
        # Checks if the Gemini API key is configured before attempting to use Gemini.
        if not GEMINI_API_KEY:
            return "ERROR: Gemini API key not configured. Cannot use Gemini models."
        try:
            # Initialize the GenerativeModel with the specified Gemini model.
            model_config = genai.GenerativeModel(CURRENT_GEMINI_MODEL)

            # Gemini API expects messages in a specific format (role and parts with text).
            # This loop converts the common message format to Gemini's expected format.
            gemini_formatted_messages = []
            for msg in messages:
                role = msg['role']
                content = msg['content']
                # Gemini uses 'model' for assistant responses, not 'assistant'.
                gemini_role = 'model' if role == 'assistant' else role
                gemini_formatted_messages.append({'role': gemini_role, 'parts': [{'text': content}]})

            # Generate content using the Gemini model.
            # request_options={'timeout': 600} sets a timeout for the API call.
            response = model_config.generate_content(
                contents=gemini_formatted_messages,
                request_options={'timeout': 600}
            )
            # Safely extract the content from the Gemini response, handling potential empty/malformed responses.
            if response and response.candidates and response.candidates[0].content and response.candidates[0].content.parts:
                return "".join(part.text for part in response.candidates[0].content.parts)
            else:
                return "ERROR: Gemini response was empty or malformed."
        except genai.APIError as e:
            # Handles errors specific to the Gemini API (e.g., invalid key, rate limits).
            print(f"Gemini API Error: {e}")
            print("Check your API key, model name, or rate limits.")
            return "ERROR: Could not get Gemini LLM response."
        except Exception as e:
            # Handles any other unexpected errors during Gemini interaction.
            print(f"An unexpected Gemini error occurred: {e}")
            return "ERROR: An unexpected Gemini error occurred."
    else:
        # Returns an error if no valid LLM source is selected.
        return "ERROR: No LLM source selected."

# --- Experiment Functions (Self-Reflection Prompts) ---
# Each experiment function follows a pattern:
# 1. Prints the experiment title and current LLM/model.
# 2. Defines an initial question and constructs the initial message list.
# 3. Gets an initial answer from the LLM.
# 4. Defines a reflection prompt designed to make the LLM self-reflect on its previous answer.
# 5. Constructs the reflection message list by appending the initial conversation to the new prompt.
# 6. Gets the self-reflected answer from the LLM.
# 7. Prints both the initial and self-reflected answers.

def run_experiment_1_1():
    """
    Experiment 1.1: Fact-Checking/Verification.
    Prompts the LLM to verify the accuracy and completeness of its initial factual answer.
    """
    print(f"\n--- Running Experiment 1.1 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "What is the primary food source of pandas, and where do they typically live?"
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Review your previous answer. While pandas primarily eat bamboo, did you accurately state the *only* thing they eat? Is there any nuance or exception to their diet?"

    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]

    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer:\n{reflection_answer}")

def run_experiment_1_2():
    """
    Experiment 1.2: Logical Consistency Check.
    Challenges the LLM to check for logical contradictions in its reasoning based on a given scenario.
    """
    print(f"\n--- Running Experiment 1.2 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    scenario = "John always walks to work. It rained heavily this morning. John is at work."
    initial_question = f"Based on the following scenario, how did John get to work?\nScenario: {scenario}"
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = f"Review your previous answer based on the scenario: '{scenario}'. Does your conclusion about how John got to work logically follow from the given facts? Are there any contradictions?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Logical Check):\n{reflection_answer}")

def run_experiment_1_3():
    """
    Experiment 1.3: Assumption Identification & Validation.
    Asks the LLM to identify and validate the underlying assumptions in its initial answer.
    """
    print(f"\n--- Running Experiment 1.3 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "How would you design a self-driving car for urban environments?"
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "In your design for a self-driving car, what key assumptions did you make about infrastructure (e.g., road markings, GPS availability), legal frameworks, or public acceptance? Are these assumptions always valid?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Assumptions):\n{reflection_answer}")

def run_experiment_2_1():
    """
    Experiment 2.1: Elaboration/Detailing.
    Prompts the LLM to provide more detailed information on a previously explained concept.
    """
    print(f"\n--- Running Experiment 2.1 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Briefly explain the concept of photosynthesis."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "That's a good brief explanation. Could you now elaborate on the specific inputs (molecules) and outputs (molecules) of photosynthesis, and where each part of the process occurs within a plant cell?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (more detail):\n{reflection_answer}")

def run_experiment_2_2():
    """
    Experiment 2.2: Addressing Omissions/Scope Expansion.
    Encourages the LLM to consider aspects or groups it might have omitted in its initial broad discussion.
    """
    print(f"\n--- Running Experiment 2.2 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Discuss the impact of social media on society."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Looking back at your discussion on social media's impact, are there any significant social groups (e.g., elderly, children, specific cultural minorities) or societal aspects (e.g., political polarization, mental health trends) whose experiences you might not have fully addressed? Expand on one such omission."
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Addressing Omissions):\n{reflection_answer}")

def run_experiment_2_3():
    """
    Experiment 2.3: Nuance and Context Addition.
    Prompts the LLM to add more nuance and context, considering specific scenarios where general statements might vary.
    """
    print(f"\n--- Running Experiment 2.3 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "What are the benefits of a plant-based diet?"
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "While a plant-based diet has many benefits, consider the nuances. How might the 'benefits' differ or require specific considerations for an elite athlete, a pregnant woman, or someone living in a region with limited access to diverse fresh produce?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Nuance):\n{reflection_answer}")

def run_experiment_3_1():
    """
    Experiment 3.1: Simplification/Clarity Check.
    Asks the LLM to simplify its explanation for a different audience while retaining core meaning.
    """
    print(f"\n--- Running Experiment 3.1 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Explain the principle of quantum entanglement."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Now, imagine you're explaining quantum entanglement to a curious 10-year-old. How would you simplify your explanation without losing the core idea?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (simplified):\n{reflection_answer}")

def run_experiment_3_2():
    """
    Experiment 3.2: Structure and Flow Optimization.
    Prompts the LLM to suggest improvements to the structure and readability of its longer-form output.
    """
    print(f"\n--- Running Experiment 3.2 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Write a short essay on the history and evolution of the internet."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Review your essay on the internet's history. Does it flow logically? Could you suggest a more effective structure (e.g., adding specific headings, reordering paragraphs) to improve readability and comprehension? Provide the suggested structure only."
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Structure Improvement):\n{reflection_answer}")

def run_experiment_3_3():
    """
    Experiment 3.3: Conciseness Review.
    Challenges the LLM to condense its detailed explanation into a concise summary.
    """
    print(f"\n--- Running Experiment 3.3 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Explain the process of photosynthesis in detail, suitable for a biology student."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Now, condense your detailed explanation of photosynthesis into a maximum of 3 concise sentences, capturing only the most crucial steps and outcomes."
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Concise):\n{reflection_answer}")

def run_experiment_4_1():
    """
    Experiment 4.1: Bias Detection and Mitigation.
    Prompts the LLM to review its description for potential biases and suggest more inclusive phrasing.
    """
    print(f"\n--- Running Experiment 4.1 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Describe the typical characteristics of a successful entrepreneur."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Review your previous description of a successful entrepreneur. Does it inadvertently lean towards any specific gender, cultural background, or personality type? How could you rephrase it to be more inclusive and neutral?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (more neutral):\n{reflection_answer}")

def run_experiment_4_2():
    """
    Experiment 4.2: Audience Appropriateness.
    Asks the LLM to adjust its tone and content for a specific, different audience.
    """
    print(f"\n--- Running Experiment 4.2 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Write a short email announcing a mandatory company-wide meeting about a new software rollout."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Now, imagine you need to deliver a similar message to a team of highly technical software engineers who are likely skeptical of new tools. How would you adjust the tone and content of your previous email to be more appropriate and persuasive for *that* specific audience?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Audience-Adjusted):\n{reflection_answer}")

def run_experiment_4_3():
    """
    Experiment 4.3: Credibility and Authority Assessment.
    Prompts the LLM to evaluate its confidence in its predictions and phrase statements with appropriate certainty.
    """
    print(f"\n--- Running Experiment 4.3 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "What is the most likely long-term impact of quantum computing on cybersecurity?"
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Review your answer on quantum computing's impact. How confident are you in these predictions? If you were to present this to a skeptical expert, how would you phrase your statements to convey appropriate levels of certainty or uncertainty, perhaps by using hedging language or acknowledging evolving research?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Credibility/Nuance):\n{reflection_answer}")

def run_experiment_5_1():
    """
    Experiment 5.1: Alternative Viewpoints/Solutions.
    Encourages the LLM to consider and propose solutions from a completely different perspective or domain.
    """
    print(f"\n--- Running Experiment 5.1 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Suggest a way to reduce plastic waste in a large office building."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "That's a good suggestion for reducing plastic waste. Now, think about this problem from a completely different angle. What is one *innovative, technology-driven* approach, or a solution focusing on *behavioral change*, that wasn't mentioned in your first answer?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (alternative):\n{reflection_answer}")

def run_experiment_5_2():
    """
    Experiment 5.2: Identifying Limitations/Edge Cases.
    Prompts the LLM to identify potential limitations or edge cases where its proposed strategy might not be effective.
    """
    print(f"\n--- Running Experiment 5.2 with {CURRENT_LLM_SOURCE}:{get_current_model_name()} ---")
    initial_question = "Propose a general strategy for effective online learning."
    initial_messages = [{'role': 'user', 'content': initial_question}]
    initial_answer = get_llm_response(initial_messages)
    print(f"Initial Answer:\n{initial_answer}\n")

    reflection_prompt = "Consider your proposed strategy for effective online learning. What are its potential limitations or edge cases? For example, where might this strategy be less effective (e.g., for certain learning disabilities, in areas with poor internet access, for highly hands-on subjects)?"
    reflection_messages = initial_messages + \
                          [{'role': 'assistant', 'content': initial_answer}] + \
                          [{'role': 'user', 'content': reflection_prompt}]
    reflection_answer = get_llm_response(reflection_messages)
    print(f"Self-Reflected Answer (Limitations/Edge Cases):\n{reflection_answer}")

# --- Helper Function ---
def get_current_model_name():
    """
    Returns the name of the currently selected LLM model based on `CURRENT_LLM_SOURCE`.
    """
    if CURRENT_LLM_SOURCE == "ollama":
        return CURRENT_OLLAMA_MODEL
    elif CURRENT_LLM_SOURCE == "gemini":
        return CURRENT_GEMINI_MODEL
    return "None"

# --- Model Selection Functions ---
def select_ollama_model():
    """
    Allows the user to select an Ollama model from a predefined list.
    Updates the `CURRENT_OLLAMA_MODEL` global variable.
    """
    global CURRENT_OLLAMA_MODEL
    print("\n--- Select Ollama Model ---")

    # List of available Ollama models.
    ollama_models = ["gemma3", "llama3", "mistral"]

    print("Available Ollama models:")
    for i, model_name in enumerate(ollama_models):
        print(f"  {i+1}. {model_name}")

    while True:
        choice = input(f"Enter number of model to use (current: {CURRENT_OLLAMA_MODEL}): ").strip()
        if choice.isdigit() and 1 <= int(choice) <= len(ollama_models):
            CURRENT_OLLAMA_MODEL = ollama_models[int(choice)-1]
            print(f"Ollama model set to: {CURRENT_OLLAMA_MODEL}")
            break
        else:
            print("Invalid choice. Please enter a valid number.")


def select_gemini_model():
    """
    Allows the user to select a Gemini model from a predefined list.
    Updates the `CURRENT_GEMINI_MODEL` global variable.
    Requires `GEMINI_API_KEY` to be set.
    """
    global CURRENT_GEMINI_MODEL
    global GEMINI_API_KEY # Access the global API key to check its presence.
    if not GEMINI_API_KEY:
        print("\nERROR: GEMINI_API_KEY environment variable not set. Cannot use Gemini models.")
        input("Press Enter to return to main menu...") # Pauses for user acknowledgment.
        return

    print("\n--- Select Gemini Model ---")
    # Dictionary of available Gemini models with corresponding numerical choices.
    gemini_options = {
        "1": "gemini-1.5-flash",
        "2": "gemini-1.5-flash-8b",
        "3": "gemini-2.0-flash",
        "4": "gemini-2.0-flash-lite",
        "5": "gemini-2.5-flash",
        "6": "gemini-2.5-flash-lite-preview-06-17",
    }

    print("Select from the following Gemini models:")
    # Prints available options sorted by their numerical key.
    for key in sorted(gemini_options.keys(), key=int):
        print(f"  {key}. {gemini_options[key]}")

    print("\nNote: 'latest' suffixes are often implied by the API or used for the newest stable version.")
    print("Some 'preview' models might have limited availability or be experimental.")

    while True:
        choice = input(f"Enter number of model to use (current: {CURRENT_GEMINI_MODEL}): ").strip()
        if choice in gemini_options:
            CURRENT_GEMINI_MODEL = gemini_options[choice]
            print(f"Gemini model set to: {CURRENT_GEMINI_MODEL}")
            break
        else:
            print("Invalid choice. Please enter a valid number from the list.")

def select_llm_source():
    """
    Allows the user to select between Ollama and Gemini as the LLM source,
    and also provides options to configure the models for each source.
    Updates the `CURRENT_LLM_SOURCE` global variable.
    """
    global CURRENT_LLM_SOURCE
    print("\n--- Select LLM Source ---")
    print(f"Current LLM Source: {CURRENT_LLM_SOURCE.upper()}")
    # Displays the currently selected model for the active source.
    if CURRENT_LLM_SOURCE == "ollama":
        print(f"Current Ollama Model: {CURRENT_OLLAMA_MODEL}")
    elif CURRENT_LLM_SOURCE == "gemini":
        print(f"Current Gemini Model: {CURRENT_GEMINI_MODEL}")

    print("\n1. Use Ollama (Local Models)")
    print("2. Use Gemini API (Cloud Models)")
    print("3. Select Ollama Model")
    print("4. Select Gemini Model")
    print("5. Back to Main Menu")

    while True:
        choice = input("Enter your choice: ").strip()
        if choice == '1':
            CURRENT_LLM_SOURCE = "ollama"
            print(f"LLM source set to Ollama. Current model: {CURRENT_OLLAMA_MODEL}")
            break
        elif choice == '2':
            CURRENT_LLM_SOURCE = "gemini"
            print(f"LLM source set to Gemini API. Current model: {CURRENT_GEMINI_MODEL}")
            if not GEMINI_API_KEY:
                print("Warning: GEMINI_API_KEY not set. Gemini models might not work.")
            break
        elif choice == '3':
            select_ollama_model()
            CURRENT_LLM_SOURCE = "ollama" # Automatically switch to Ollama if configured.
            break
        elif choice == '4':
            select_gemini_model()
            CURRENT_LLM_SOURCE = "gemini" # Automatically switch to Gemini if configured.
            break
        elif choice == '5':
            break # Return to main menu.
        else:
            print("Invalid choice. Please enter 1, 2, 3, 4, or 5.")


# --- Experiment Menu Data Structure ---
# A dictionary mapping numerical choices to experiment details (name and function).
experiments_menu = {
    "1.1": {"name": "Fact-Checking/Verification", "function": run_experiment_1_1},
    "1.2": {"name": "Logical Consistency Check", "function": run_experiment_1_2},
    "1.3": {"name": "Assumption Identification & Validation", "function": run_experiment_1_3},
    "2.1": {"name": "Elaboration/Detailing", "function": run_experiment_2_1},
    "2.2": {"name": "Addressing Omissions/Scope Expansion", "function": run_experiment_2_2},
    "2.3": {"name": "Nuance and Context Addition", "function": run_experiment_2_3},
    "3.1": {"name": "Simplification/Clarity Check", "function": run_experiment_3_1},
    "3.2": {"name": "Structure and Flow Optimization", "function": run_experiment_3_2},
    "3.3": {"name": "Conciseness Review", "function": run_experiment_3_3},
    "4.1": {"name": "Bias Detection and Mitigation", "function": run_experiment_4_1},
    "4.2": {"name": "Audience Appropriateness", "function": run_experiment_4_2},
    "4.3": {"name": "Credibility and Authority Assessment", "function": run_experiment_4_3},
    "5.1": {"name": "Alternative Viewpoints/Solutions", "function": run_experiment_5_1},
    "5.2": {"name": "Identifying Limitations/Edge Cases", "function": run_experiment_5_2},
}

# --- User Interface Functions ---
def display_main_menu():
    """
    Displays the main menu of the LLM self-reflection experiment suite,
    showing current LLM source and model, and available experiments.
    """
    print("\n--- LLM Self-Reflection Experiments Menu ---")
    print(f"**Current LLM Source: {CURRENT_LLM_SOURCE.upper()}**")
    if CURRENT_LLM_SOURCE == "ollama":
        print(f"**Current Ollama Model: {CURRENT_OLLAMA_MODEL}**")
    elif CURRENT_LLM_SOURCE == "gemini":
        print(f"**Current Gemini Model: {CURRENT_GEMINI_MODEL}**")

    print("\n--- Options ---")
    print("S: Select LLM Source & Model")
    print("\n--- Experiments ---")
    # Categorized display of experiments for better readability.
    print("\n--- 1. Enhancing Accuracy and Correctness ---")
    print("  1.1: Fact-Checking/Verification")
    print("  1.2: Logical Consistency Check")
    print("  1.3: Assumption Identification & Validation")
    print("\n--- 2. Improving Completeness and Depth ---")
    print("  2.1: Elaboration/Detailing")
    print("  2.2: Addressing Omissions/Scope Expansion")
    print("  2.3: Nuance and Context Addition")
    print("\n--- 3. Enhancing Clarity and Readability ---")
    print("  3.1: Simplification/Clarity Check")
    print("  3.2: Structure and Flow Optimization")
    print("  3.3: Conciseness Review")
    print("\n--- 4. Refining Perspective and Tone ---")
    print("  4.1: Bias Detection and Mitigation")
    print("  4.2: Audience Appropriateness")
    print("  4.3: Credibility and Authority Assessment")
    print("\n--- 5. Stimulating Creativity and Alternative Solutions ---")
    print("  5.1: Alternative Viewpoints/Solutions")
    print("  5.2: Identifying Limitations/Edge Cases")
    print("\n-------------------------------------------")
    print("Type 'exit' to quit.")
    print("-------------------------------------------\n")

# --- Main Application Logic ---
def main():
    """
    The main function that runs the LLM self-reflection experiment suite.
    It displays a menu, handles user input, and executes selected experiments or configurations.
    """
    print("Welcome to the LLM Self-Reflection Experiment Suite!")
    if not GEMINI_API_KEY:
        print("\nIMPORTANT: To use Gemini models, please set your GEMINI_API_KEY environment variable.")
        print("You can get an API key from https://ai.google.dev/.")
    print(f"Current default LLM: {CURRENT_LLM_SOURCE.upper()}, Model: {get_current_model_name()}")
    time.sleep(2) # Provides a short pause for the user to read initial messages.

    while True:
        display_main_menu()
        choice = input("Enter option (S for selection, experiment number e.g., 1.1): ").strip().lower()

        if choice == 'exit' or choice == 'e':
            print("Exiting experiments. Goodbye!")
            break
        elif choice == 's':
            select_llm_source() # Calls the function to select LLM source and models.
        elif choice in experiments_menu:
            print(f"Running: {experiments_menu[choice]['name']}...")
            experiments_menu[choice]["function"]() # Executes the selected experiment function.
            print("\n--- Experiment Complete ---")
            input("\nPress Enter to return to main menu...") # Pauses after each experiment.
        else:
            print("Invalid choice. Please enter 'S', a valid experiment number, or 'exit'.")
        print("\n" + "="*70 + "\n") # Separator for better readability between menu cycles.

# Entry point for the script.
if __name__ == "__main__":
    main()