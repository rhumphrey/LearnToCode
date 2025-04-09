import requests
import json

def get_api_response(text):
    
    url = "http://localhost:11434/api/chat"
    payload = {
        "model": "llama3.2",
        "messages": [{"role": "user", "content": text}],
        "stream": False,
    }
    headers = {"Content-Type": "application/json"}
    return requests.post(url, data=json.dumps(payload), headers=headers)

def display_api_content(api_response):
        
    if api_response is not None and api_response.status_code == 200:
        print(api_response.json()['message']['content'])
    else:
        print(f"Error: {api_response.status_code}, {api_response.text}")

def main():
       
    user_input = "Explain Mixture of Experts"
    print("Doing a Thunk!\n\n")
    api_response = get_api_response(user_input)
    display_api_content(api_response)

if __name__ == "__main__":
    main()