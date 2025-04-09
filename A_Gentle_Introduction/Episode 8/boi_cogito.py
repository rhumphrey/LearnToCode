import requests
import json
import datetime
import os
import re  # Import the regular expression module

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

    if deep_thinking:
        # Prepend a system message to enable deep thinking if the flag is True
        messages.insert(0, {"role": "system", "content": "Enable deep thinking subroutine."})


    payload = {
        "model": "cogito",
        "messages": messages,
        "stream": False,
    }
    headers = {"Content-Type": "application/json"}
    return requests.post(url, data=json.dumps(payload), headers=headers)


def display_api_content(api_response: requests.Response) -> str:
    """Displays the content of a successful API response and returns it.

    Args:
        api_response: The requests.Response object returned by the get_api_response function.

    Returns:
        The content of the API response as a string, or an error message.
    """
    if api_response is not None and api_response.status_code == 200:
        return api_response.json()['message']['content']
    else:
        return f"Error: {api_response.status_code}, {api_response.text}"

def save_response_to_md(question: str, response_content: str) -> None:
    """Saves the API response content to a .md file with a name based on the question.

    Args:
        question: The user's question used to generate the filename.
        response_content: The text content of the API response to be saved.
    """
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    # Sanitize the question to create a valid filename
    sanitized_question = re.sub(r'[^\w\s-]', '', question).strip().replace(' ', '_')
    filename = f"{sanitized_question}_{timestamp}.md"
    try:
        with open(filename, "w") as f:
            f.write(f"# Question: {question}\n\n")
            f.write(f"**Timestamp:** {timestamp}\n\n")
            f.write("## Response:\n\n")
            f.write(response_content)
        print(f"\nResponse saved to: {os.path.abspath(filename)}\n")
    except Exception as e:
        print(f"Error saving response to file: {e}")


def main() -> None:
    """
    Main function to get user input, enable deep thinking if requested,
    send the input to the API, display the response, and save it to a Markdown file.
    """
    user_input = input("Enter your question: ")
    deep_thinking_enabled = input("Enable deep thinking? (yes/no): ").lower() == "yes"

    thinking_message = "Thinking Deeply..." if deep_thinking_enabled else "Thinking Normally..."
    print(f"\n{thinking_message}\n")

    api_response = get_api_response(user_input, deep_thinking=deep_thinking_enabled)
    response_content = display_api_content(api_response)

    if isinstance(response_content, str) and not response_content.startswith("Error"):
        print("\n--- API Response ---")
        print(response_content)
        save_response_to_md(user_input, response_content)
    else:
        print(f"\n--- API Error ---")
        print(response_content)


if __name__ == "__main__":
    # This block ensures that the main function is executed only when the script is run directly.
    main()