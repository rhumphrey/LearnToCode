import ollama
import json
import time
from datetime import datetime

# --- Setup Instructions ---
# 1. Ensure Ollama is running.
# 2. Ensure you have downloaded at least one model using `ollama pull <model_name>`. So, for gpt-oss-20b: `ollama pull gpt-oss-20b`
# 3. Install the Ollama Python library: `pip install ollama`.

def run_experiment(client, model, messages, description, file_handle):
    """
    Helper function to run a single experiment and print/save the results.
    """
    print(f"--- Running Experiment: {description} ---")
    file_handle.write(f"### {description}\n")
    try:
        response = client.chat(model=model, messages=messages)
        content = response['message']['content']
        print(content)
        file_handle.write("\n" + content + "\n\n\n")
    except Exception as e:
        error_message = f"An error occurred: {e}"
        print(error_message)
        file_handle.write("```\n" + error_message + "\n```\n\n")
    file_handle.write("\n\n")
    print("\n\n")

def run_all_experiments(model_name):
    """
    Main function to run all experiments and save the results to a Markdown file.
    The model to be tested is passed as an argument.
    """
    # Create a date-stamped filename for the results, including the model name
    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    filename = f"ollama_experiments_results_{model_name.replace(':', '-')}_{timestamp}.md"

    # Initialize the Ollama client
    client = ollama.Client()

    print(f"--- Running experiments for model: {model_name} ---")

    with open(filename, 'w', encoding='utf-8') as f:
        f.write(f"# Ollama Experiment Results for {model_name} - {timestamp}\n\n")
        f.write("This file contains the results of the experiments designed to evaluate the capabilities of the specified Ollama model.\n\n")
        f.write("--- Running Original 10 Experiments ---\n\n")

        # --- 1. Variable Effort Reasoning ---
        reasoning_problem = "A car travels at a constant speed of 60 mph for 2.5 hours. How far does it travel?"
        run_experiment(client, model_name, [
            {'role': 'system', 'content': 'Reasoning: low'},
            {'role': 'user', 'content': f'Solve the following math problem step-by-step: {reasoning_problem}'}
        ], "1. Variable Effort Reasoning (Low)", f)

        run_experiment(client, model_name, [
            {'role': 'system', 'content': 'Reasoning: medium'},
            {'role': 'user', 'content': f'Solve the following math problem step-by-step: {reasoning_problem}'}
        ], "2. Variable Effort Reasoning (Medium)", f)

        run_experiment(client, model_name, [
            {'role': 'system', 'content': 'Reasoning: high'},
            {'role': 'user', 'content': f'Solve the following math problem step-by-step: {reasoning_problem}'}
        ], "3. Variable Effort Reasoning (High)", f)

        # --- 2. Python Code Generation ---
        code_generation_prompt = "Write a Python function that takes a list of numbers and returns a new list with only the even numbers."
        run_experiment(client, model_name, [
            {'role': 'system', 'content': 'You are an expert Python programmer.'},
            {'role': 'user', 'content': code_generation_prompt}
        ], "4. Python Code Generation", f)

        # --- 3. Carbon Code Generation ---
        code_generation_prompt = "Write a Carbon function that takes a list of numbers and returns a new list with only the even numbers."
        run_experiment(client, model_name, [
            {'role': 'system', 'content': 'You are an expert Carbon programmer.'},
            {'role': 'user', 'content': code_generation_prompt}
        ], "5. Carbon Code Generation", f)

        # --- 4. Multilingual Translation ---
        run_experiment(client, model_name, [
            {'role': 'user', 'content': 'Translate the following English sentence to Spanish: "The quick brown fox jumps over the lazy dog."'}
        ], "6. Multilingual Translation", f)

        # --- 5. Creative Writing and Style Transfer ---
        creative_writing_prompt = "Write a short, three-paragraph story about a robot who discovers an abandoned garden, written in the style of Ernest Hemingway."
        run_experiment(client, model_name, [
            {'role': 'user', 'content': creative_writing_prompt}
        ], "7. Creative Writing and Style Transfer", f)

        # --- 6. Text Summarization ---
        long_text = """
        The human brain is a complex organ of the central nervous system. It has two main parts: the cerebrum and the cerebellum. The cerebrum is the largest part of the brain and is responsible for functions like thought, language, and voluntary movement. The cerebellum, located at the back of the brain, is crucial for coordination, balance, and posture. Neurons, or nerve cells, are the fundamental units of the brain and nervous system, transmitting electrical signals to communicate with each other. The brain's plasticity, or ability to adapt and change over time, allows it to learn and form new memories.
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': f'Summarize the key findings from the following text: {long_text}'}
        ], "8. Text Summarization", f)

        # --- 7. Factuality and Knowledge Cutoff ---
        run_experiment(client, model_name, [
            {'role': 'user', 'content': 'Who was the winner of the men\'s singles title at Wimbledon in July 2025?'}
        ], "9. Factuality and Knowledge Cutoff", f)

        # --- 8. Logical Problem Solving ---
        logic_puzzle = "There are three switches outside a windowless room. One of the switches turns on a light bulb inside the room. You can only enter the room once. How can you determine which switch operates the light?"
        run_experiment(client, model_name, [
            {'role': 'user', 'content': logic_puzzle}
        ], "10. Logical Problem Solving", f)

        # --- 9. Health-related Query ---
        run_experiment(client, model_name, [
            {'role': 'user', 'content': 'What are some common symptoms of a vitamin D deficiency?'}
        ], "11. Health-related Query", f)

        # --- 10. Multi-turn Conversation ---
        print("--- Running Experiment: 12. Multi-turn Conversation ---")
        f.write("### 12. Multi-turn Conversation\n")
        try:
            conversation_history = []
            
            prompt1 = "What is the capital of France?"
            conversation_history.append({'role': 'user', 'content': prompt1})
            response1 = client.chat(model=model_name, messages=conversation_history)
            print(f"User: {prompt1}")
            f.write(f"**User:** {prompt1}\n")
            print(f"Model: {response1['message']['content']}")
            f.write(f"**Model:** {response1['message']['content']}\n\n")
            conversation_history.append(response1['message'])
            
            time.sleep(1) 
            
            prompt2 = "And what is the capital of Japan?"
            conversation_history.append({'role': 'user', 'content': prompt2})
            response2 = client.chat(model=model_name, messages=conversation_history)
            print(f"User: {prompt2}")
            f.write(f"**User:** {prompt2}\n")
            print(f"Model: {response2['message']['content']}")
            f.write(f"**Model:** {response2['message']['content']}\n\n")
            
        except Exception as e:
            error_message = f"An error occurred: {e}"
            print(error_message)
            f.write("```\n" + error_message + "\n```\n\n")
        f.write("\n\n")
        print("\n\n")

             
        # --- 11. Code Debugging and Explanation ---
        debugging_prompt = """
        The following Python function is supposed to sum all the numbers in a list, but it's not working correctly. Find the bug, fix the code, and explain your changes.

        def sum_list(numbers):
            total = 0
            for i in range(len(numbers)):
                total = numbers[i]
            return total
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': debugging_prompt}
        ], "13. Code Debugging and Explanation", f)

        # --- 12. Multi-Part Instruction Following ---
        multipart_instruction_prompt = """
        Write a two-paragraph story about a cat who can talk. In the first paragraph, the cat must be in a library. In the second paragraph, the cat must meet a dog who is afraid of books. The story must end with the cat quoting a famous author.
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': multipart_instruction_prompt}
        ], "14. Multi-Part Instruction Following", f)

        # --- 13. Long Context Question Answering ---
        long_context_text = """
        The history of the internet dates back to the development of early electronic computers. The United States government funded research in the 1960s to build robust, fault-tolerant computer networks. This research led to the ARPANET (Advanced Research Projects Agency Network), the first wide-area packet-switching network. The primary purpose of ARPANET was to allow research institutions to share computing resources and to create a decentralized communication system that could withstand potential attacks. It was designed to be a network of geographically dispersed computers that could exchange information seamlessly. Over the next two decades, the network grew, and key protocols were developed, such as TCP/IP, which formed the foundation of the modern internet. The transition from ARPANET to the internet as we know it began in the 1980s, when the NSFNET (National Science Foundation Network) took over the primary backbone. The World Wide Web, developed by Tim Berners-Lee in the late 1980s and early 1990s, made the internet accessible to the general public through a graphical interface, leading to its massive expansion.
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': f'Read the following article about the history of the internet. Then, answer the question: What was the primary purpose of ARPANET?\n\n{long_context_text}'}
        ], "15. Long Context Question Answering", f)

        # --- 14. Counterfactual Reasoning ---
        run_experiment(client, model_name, [
            {'role': 'user', 'content': 'If the internet had been invented in the 15th century, how might it have affected the Renaissance?'}
        ], "16. Counterfactual Reasoning", f)

        # --- 15. Data Extraction from Unstructured Text ---
        data_extraction_prompt = """
        Extract the company name, job title, and location from the following job description and format the result as a JSON object: "We're hiring a Senior Software Engineer in our San Francisco office. Join Google's cutting-edge AI research team."
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': data_extraction_prompt}
        ], "17. Data Extraction from Unstructured Text", f)

        # --- 16. Code Refactoring ---
        refactoring_prompt = """
        Refactor the following Python code to be more concise and "Pythonic" using a list comprehension:
            
        numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        even_numbers = []
        for number in numbers:
            if number % 2 == 0:
                even_numbers.append(number)
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': refactoring_prompt}
        ], "18. Code Refactoring", f)

        # --- 17. Code-Switching Translation ---
        print("--- Running Experiment: 19. Code-Switching Translation ---")
        f.write("### 19. Code-Switching Translation\n")
        try:
            conversation_history = []
            
            prompt1 = "I need to go to the store."
            conversation_history.append({'role': 'user', 'content': prompt1})
            response1 = client.chat(model=model_name, messages=conversation_history)
            print(f"User: {prompt1}")
            f.write(f"**User:** {prompt1}\n")
            print(f"Model: {response1['message']['content']}")
            f.write(f"**Model:** {response1['message']['content']}\n\n")
            conversation_history.append(response1['message'])
            
            time.sleep(1) 
            
            prompt2 = "Necesito comprar pan y leche."
            conversation_history.append({'role': 'user', 'content': prompt2})
            response2 = client.chat(model=model_name, messages=conversation_history)
            print(f"User: {prompt2}")
            f.write(f"**User:** {prompt2}\n")
            print(f"Model: {response2['message']['content']}")
            f.write(f"**Model:** {response2['message']['content']}\n\n")
            
        except Exception as e:
            error_message = f"An error occurred: {e}"
            print(error_message)
            f.write("```\n" + error_message + "\n```\n\n")
        f.write("\n\n")
        print("\n\n")


        # --- 18. Creating a Persona-based Response ---
        run_experiment(client, model_name, [
            {'role': 'system', 'content': 'Act as a cynical film critic from the 1950s.'},
            {'role': 'user', 'content': 'Give me your review of the film "Oppenheimer" (2023).'}
        ], "20. Creating a Persona-based Response", f)

        # --- 19. Creative Writing with Constraints ---
        constrained_writing_prompt = """
        Write a five-sentence poem about a forgotten key. The poem must use the words "rust," "silence," and "memory." The last word of the poem must be "door."
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': constrained_writing_prompt}
        ], "21. Creative Writing with Constraints", f)

        # --- 20. Abstract Visual Description ---
        abstract_description_prompt = """
        Describe the concept of "time" as if it were a physical object you could hold in your hands. What would it look like, feel like, and what would it do?
        """
        run_experiment(client, model_name, [
            {'role': 'user', 'content': abstract_description_prompt}
        ], "22. Abstract Visual Description", f)

    print(f"All experiments finished. Results saved to {filename}")

if __name__ == "__main__":
    try:
        # Get list of local models
        client = ollama.Client()
        models = client.list()['models']
        
        if not models:
            print("No models found. Please download at least one model with 'ollama pull <model_name>'")
        else:
            print("Available models:")
            for i, model in enumerate(models):
                print(f"{i+1}. {model['model']}")
            
            # User selects a model
            while True:
                choice_input = input("Enter the number of the model you want to test (or 'q' to quit): ")
                
                if choice_input.lower() == 'q':
                    print("Exiting program.")
                    break
                    
                try:
                    choice = int(choice_input)
                    if 1 <= choice <= len(models):
                        selected_model_name = models[choice-1]['model']
                        run_all_experiments(selected_model_name)
                        break
                    else:
                        print("Invalid choice. Please enter a number from the list.")
                except ValueError:
                    print("Invalid input. Please enter a number or 'q'.")
                    
    except Exception as e:
        print(f"An error occurred while trying to connect to Ollama: {e}")

