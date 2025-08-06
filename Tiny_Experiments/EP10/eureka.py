import ollama
import json


# ==============================================================================
# Setup and Configuration
# ==============================================================================

# IMPORTANT: You must have Ollama running and the specified model pulled.
# To install Ollama, visit https://ollama.com.
# To pull the model used here, run `ollama pull llama3.1` in your terminal.
# To install the Python library, run `pip install ollama`.

# The LLM model to use. 'llama3.1' is a great all-purpose choice.
# You can change this to any model you have pulled, e.g., 'mistral', 'codellama'.
MODEL = "llama3.1"

def run_experiment(experiment_number: int, experiment_title: str, prompt: str, model_name: str = MODEL) -> None:
    """
    Executes a single experiment by sending a prompt to the Ollama model
    and printing the response.
    """
    print()
    print("=" * 80)
    print(f"EXPERIMENT {experiment_number}: {experiment_title.upper()}")
    print("-" * 80)
    print(f"Prompt:\n{prompt}\n")

    try:
        # Use the ollama.chat function for a conversational context.
        # This allows us to maintain a memory, though each experiment
        # in this script is treated as a new conversation for clarity.
        response = ollama.chat(
            model=model_name,
            messages=[{'role': 'user', 'content': prompt}],
            stream=False # We get the full response at once.
        )
        # Check if the response contains the expected content
        if 'message' in response and 'content' in response['message']:
            print("Response:\n" + response['message']['content'])
        else:
            print("Error: The response from Ollama did not contain the expected message content.")
            print(f"Full response: {json.dumps(response, indent=2)}")

    except Exception as e:
        print(f"An error occurred: {e}")
        print("Make sure the Ollama server is running and the model is available.")


# ==============================================================================
# The Eureka Experiments
# ==============================================================================

if __name__ == "__main__":
    print(f"\n--- Running 10 Eureka Experiments with the '{MODEL}' model ---")
    print("These experiments are designed to spark creative and inspiring ideas.")
    print("They explore different facets of the LLM's capabilities, from logic to art.")
    print("Make sure your Ollama server is running and the model is ready.\n")

    # Experiment 1: Role-Playing a Non-Human Entity
    # Can the LLM adopt a creative, non-human persona and provide a unique perspective?
    run_experiment(
        1,
        "See from a Different View",
        "Imagine you are the oldest, wisest tree in a magical forest. A small bird lands on your branch and asks you for the most important life lesson you have learned. What do you tell it?"
    )

    # Experiment 2: Creative Writing with a Constraint
    # This tests the model's ability to be creative while adhering to strict rules.
    run_experiment(
        2,
        "Constrained Creativity",
        "Write a 5-line rhyming poem about a spaceship, but every line must end with a word that is also a planet's name (e.g., Saturn, Mars, etc.)."
    )

    # Experiment 3: Logical Abstraction and Puzzle Solving
    # Can the model break down a problem and apply logical reasoning?
    run_experiment(
        3,
        "Logic and Abstraction",
        "Explain the solution to the classic 'fox, goose, and beans' river crossing puzzle. Present the steps in a clear, numbered list."
    )

    # Experiment 4: Code Generation for a Quirky Task
    # Pushes the model's coding abilities to solve a non-standard problem.
    run_experiment(
        4,
        "Coding for the Absurd",
        "Write a Python function that takes a string and returns a new string where every vowel is replaced with a random emoji. The function should be well-commented and include a simple example usage."
    )

    # Experiment 5: Ideation for a Fictional Product
    # This demonstrates the LLM's use as a brainstorming partner.
    run_experiment(
        5,
        "The Ideation Partner",
        "You are a product designer. Brainstorm three innovative features for a smart teapot that go beyond just boiling water. For each feature, provide a name and a short description of its function and benefit."
    )

    # Experiment 6: Historical Reinterpretation
    # Tests the model's ability to reinterpret history from an unexpected angle.
    run_experiment(
        6,
        "Historical Reimagination",
        "Recount the story of the discovery of electricity, but from the perspective of a squirrel who was accidentally part of Benjamin Franklin's kite experiment. Make it dramatic and a little paranoid."
    )

    # Experiment 7: De-jargonization and Simplification
    # This shows how an LLM can make complex topics accessible to a wider audience.
    run_experiment(
        7,
        "Clarifying Complexity",
        "Explain the concept of quantum entanglement to a child who is just learning about magnets. Use simple analogies and avoid complex scientific terms."
    )

    # Experiment 8: A "What If" Scenario
    # Encourages the model to think creatively and logically about a hypothetical situation.
    run_experiment(
        8,
        "Hypothetical Reasoning",
        "What if humanity's only form of communication was through music? Describe how a complex concept, like a legal contract or a scientific discovery, would be conveyed."
    )

    # Experiment 9: Generating a Metaphorical Explanation
    # Can the model create a compelling and creative metaphor for an abstract concept?
    run_experiment(
        9,
        "Metaphorical Expression",
        "Create a metaphor to explain how machine learning models learn. Use the analogy of a chef learning to cook new dishes. Be detailed and descriptive."
    )

    # Experiment 10: The Unsolvable Prompt
    # This tests the model's ability to handle ambiguous or impossible requests gracefully and creatively.
    run_experiment(
        10,
        "Confronting the Impossible",
        "What does the number five smell like? Be creative, poetic, and don't say it has no smell."
    )
    print("\n--- All experiments completed ---")
    print("I hope these examples have given you some new ideas and a fresh perspective on what LLMs can do.")

