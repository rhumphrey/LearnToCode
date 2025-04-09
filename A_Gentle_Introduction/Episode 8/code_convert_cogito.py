import requests
import json
import datetime
import os

def get_api_response(text: str, deep_thinking: bool = False) -> requests.Response:
    """
    Sends a request to a local API endpoint for chat.

    Args:
        text: The user's input text to send to the API.
        deep_thinking: A boolean flag indicating whether to enable a "deep thinking" mode.
                       If True, a special system message is prepended to the messages. Defaults to False.

    Returns:
        The requests.Response object from the API call.
    """
    url = "http://localhost:11434/api/chat"
    messages = [{"role": "user", "content": text}]

    # Conditionally add the system message for deep thinking
    if deep_thinking:
        messages.insert(0, {"role": "system", "content": "Enable deep thinking subroutine for code conversion."})

    payload = {
        "model": "cogito",
        "messages": messages,
        "stream": False,
    }
    headers = {"Content-Type": "application/json"}
    return requests.post(url, data=json.dumps(payload), headers=headers)


def display_api_content(api_response: requests.Response) -> str:
    """Displays the content of a successful API response.

    Args:
        api_response: The requests.Response object returned by the get_api_response function.

    Returns:
        The content of the API response as a string, or an error message.
    """
    if api_response is not None and api_response.status_code == 200:
        return api_response.json()['message']['content']
    else:
        return f"Error: {api_response.status_code}, {api_response.text}"


def save_response_to_file(response_content: str, original_language: str, target_language: str) -> None:
    """Saves the API response content to a .md (Markdown) file.

    Args:
        response_content: The text content of the API response to be saved.
        original_language: The original programming language of the code.
        target_language: The target programming language for the conversion.
    """
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"converted_code_{original_language}_to_{target_language}_{timestamp}.md"
    try:
        with open(filename, "w") as f:
            f.write(f"# Code Conversion\n\n")
            f.write(f"**Original Language:** {original_language}\n")
            f.write(f"**Target Language:** {target_language}\n\n")
            f.write("## Converted Code:\n\n")
            f.write(response_content)
        print(f"\nResponse saved to: {os.path.abspath(filename)}\n")
    except Exception as e:
        print(f"Error saving response to file: {e}")


def main() -> None:
    """
    Main function to get user input for code conversion, send the request to the API,
    display the response, and save it to a Markdown file.
    """
    while True:
        print("\n--- Code Conversion Tool ---")
        original_language = input("Enter the original programming language: ").strip()
        code_to_convert = input("Paste the code you want to convert:\n").strip()
        target_language = input("Enter the target programming language: ").strip()
        deep_thinking_enabled = input("Enable deep thinking for conversion? (yes/no): ").lower() == "yes"

        # Construct the prompt
        prompt = f"Please convert the following code from {original_language} to {target_language}:\n\n```{original_language}\n{code_to_convert}\n```\n\nPlease provide the converted code in {target_language} and ensure it is functional and adheres to best practices."

        # Conditional thinking message
        thinking_message = "Thinking Deeply about the code conversion..." if deep_thinking_enabled else "Thinking about the code conversion..."
        print(f"\n{thinking_message}\n")

        # Get API response
        print("Sending request to the API...")
        print("Waiting for the model to respond (this could take several minutes)...")
        api_response = get_api_response(prompt, deep_thinking=deep_thinking_enabled)
        


        # Display and save response
        if api_response.status_code == 200:
            print("Response received successfully!\n")
            response_content = display_api_content(api_response)
            print("--- Converted Code ---")
            print(response_content)
            save_response_to_file(response_content, original_language, target_language)
        else:
            print("API request failed.")
            display_api_content(api_response)

        repeat = input("Do you want to perform another conversion? (yes/no): ").lower()
        if repeat != "yes":
            print("Exiting the Code Conversion Tool. Goodbye!")
            break

if __name__ == "__main__":
    # This block ensures that the main function is executed only when the script is run directly.
    main()