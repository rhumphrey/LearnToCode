import ollama
import google.generativeai as genai
import time
import datetime
import os
from typing import List, Dict

# --- Configuration ---
class Config:
    """Application configuration."""
    OLLAMA_MODEL_A = 'qwen2.5:0.5b'
    OLLAMA_MODEL_B = 'qwen2.5:7b'
    GEMINI_API_KEY = os.environ.get('GEMINI_API_KEY')
    GEMINI_MODEL = 'gemini-2.5-flash'
    WAIT_TIME_SECONDS = 5
    OUTPUT_FILENAME = "ab_test_report.md"

class LLMService:
    """Manages interactions with different LLM APIs."""
    def __init__(self, ollama_client: ollama.Client, gemini_client: genai.GenerativeModel):
        self.ollama_client = ollama_client
        self.gemini_client = gemini_client

    def get_ollama_response(self, model: str, prompt: str) -> str:
        """Fetches a response from an Ollama model."""
        try:
            response = self.ollama_client.generate(model=model, prompt=prompt, stream=False)
            return response['response']
        except Exception as e:
            return f"Error with {model}: {e}"

    def assess_with_gemini(self, task: str, response_a: str, response_b: str, model_a: str, model_b: str) -> str:
        """Assesses two responses using the Gemini API."""
        prompt = f"""
        You are an expert AI evaluator. Your task is to compare two responses, labeled 'Response A' and 'Response B', to the following user prompt.

        User Prompt:
        {task}

        Response A (from {model_a}):
        {response_a}

        Response B (from {model_b}):
        {response_b}

        Compare the two responses based on the following criteria:
        1. **Relevance**: How well does the response address the user prompt?
        2. **Clarity**: Is the response easy to understand and well-structured?
        3. **Completeness**: Does the response fully answer the prompt?
        4. **Creativity/Quality**: For creative tasks, how original is the response? For factual tasks, how accurate is it?

        Provide a score for each response from 1 to 10 (10 being the best) for each criterion. Then, provide a final overall assessment and state which response is better, and why. Be objective and provide clear reasoning.
        """
        try:
            response = self.gemini_client.generate_content(prompt)
            if response.text:
                return response.text
            else:
                return "Gemini assessment response was empty."
        except Exception as e:
            return f"Error with Gemini API: {e}"

class ReportGenerator:
    """Handles writing test results to a markdown file."""
    def __init__(self, filename: str):
        self.filename = filename

    def initialize_report(self, models: Dict[str, str], assessment_model: str):
        """Creates or overwrites the markdown report header."""
        with open(self.filename, "a", encoding="utf-8") as f:
            f.write(f"# LLM A/B Test Report\n\n")
            f.write(f"**Date:** {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            f.write(f"**Models Tested:**\n")
            for key, value in models.items():
                f.write(f"- **{key}:** `{value}`\n")
            f.write(f"\n**Assessment Model:** `{assessment_model}`\n\n---\n\n")

    def log_test_result(self, test_number: int, task: str, results: Dict[str, str]):
        """Appends a single test's results to the report."""
        markdown_content = f"## Test {test_number}: {task}\n\n"
        for key, value in results.items():
            markdown_content += f"### {key}\n\n{value}\n\n\n"
        markdown_content += "---\n\n"
        
        with open(self.filename, "a", encoding="utf-8") as f:
            f.write(markdown_content)

def main():
    """Main function to orchestrate the A/B testing process."""
    
    # --- Test Panel ---
    TEST_TASKS = [
        "Write a short, creative story about a robot who finds a lost cat.",
        "Explain the concept of 'quantum entanglement' in simple terms.",
        "Draft a professional email to a colleague requesting an update on a project.",
        "Provide a detailed recipe for chocolate chip cookies.",
        "Translate the phrase 'Hello, how are you?' into French, Spanish, and German.",
        "Summarize the plot of the movie 'Inception'.",
        "List three benefits of a plant-based diet.",
        "Write a Python function to check if a number is prime.",
        "Compose a simple poem about a starry night.",
        "Answer the following riddle: I have cities, but no houses. I have mountains, but no trees. I have water, but no fish. What am I?"
    ]

    # --- Setup ---
    # Retrieve API key from environment variables
    if not Config.GEMINI_API_KEY:
        print("Error: The GEMINI_API_KEY environment variable is not set.")
        print("Please set the variable and try again.")
        return # Exit the program gracefully

    genai.configure(api_key=Config.GEMINI_API_KEY)
    llm_service = LLMService(
        ollama_client=ollama.Client(),
        gemini_client=genai.GenerativeModel(Config.GEMINI_MODEL)
    )
    report_generator = ReportGenerator(Config.OUTPUT_FILENAME)

    # --- Execution ---
    print("Starting A/B test between Ollama models...\n")
    report_generator.initialize_report(
        models={"Model A": Config.OLLAMA_MODEL_A, "Model B": Config.OLLAMA_MODEL_B},
        assessment_model=Config.GEMINI_MODEL
    )

    for i, task in enumerate(TEST_TASKS):
        print(f"--- Running Test {i+1}/{len(TEST_TASKS)} ---")
        print(f"Task: {task}")
        
        # Get responses
        response_a = llm_service.get_ollama_response(Config.OLLAMA_MODEL_A, task)
        response_b = llm_service.get_ollama_response(Config.OLLAMA_MODEL_B, task)
        
        # Assess responses
        gemini_assessment = llm_service.assess_with_gemini(
            task, response_a, response_b, Config.OLLAMA_MODEL_A, Config.OLLAMA_MODEL_B
        )
        
        # Log results
        print("\nOllama Model A Response:\n", response_a)
        print("\nOllama Model B Response:\n", response_b)
        print("\nGemini Assessment:\n", gemini_assessment)
        print("\n---\n")

        results_to_log = {
            f"Model A ({Config.OLLAMA_MODEL_A}) Response": response_a,
            f"Model B ({Config.OLLAMA_MODEL_B}) Response": response_b,
            "Gemini Assessment": gemini_assessment
        }
        report_generator.log_test_result(i + 1, task, results_to_log)
        
        # Wait
        if i < len(TEST_TASKS) - 1:
            print(f"Waiting for {Config.WAIT_TIME_SECONDS} seconds before the next test...\n")
            time.sleep(Config.WAIT_TIME_SECONDS)
            
    print(f"A/B test completed. Results saved to {Config.OUTPUT_FILENAME}")

if __name__ == "__main__":
    main()