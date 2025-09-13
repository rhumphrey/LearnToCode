import requests
import os
import json
import datetime
import re
from pathlib import Path

# It is highly recommended to set your API key as an environment variable
API_KEY = os.getenv("GEMINI_API_KEY")

GEMINI_API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"

# Create analysis folder if it doesn't exist
analysis_folder = "code_analysis"
if not os.path.exists(analysis_folder):
    os.makedirs(analysis_folder)

def detect_language_with_gemini(code, filename):
    """Use Gemini to detect the programming language and find similar languages"""
    
    prompt = f"""
    Analyze the following code snippet and determine the primary programming language it is written in.
    Also, identify four other programming languages that are syntactically similar to this code.
    
    Filename: {filename}
    Code:
    ```
    {code[:2000]}  # Limit code length for detection
    ```
    
    Please respond in the following JSON format:
    {{
        "primary_language": "Language Name",
        "similar_languages": ["Language1", "Language2", "Language3", "Language4"],
        "confidence": "High/Medium/Low"
    }}
    """
    
    headers = {
        "x-goog-api-key": API_KEY,
        "Content-Type": "application/json"
    }
    
    request_payload = {
        "contents": [
            {
                "parts": [{"text": prompt}]
            }
        ],
        "generationConfig": {
            "temperature": 0.1,  # Low temperature for more deterministic output
            "topP": 0.8,
            "topK": 64,
        }
    }
    
    try:
        print("Detecting programming language with Gemini...")
        response = requests.post(GEMINI_API_ENDPOINT, headers=headers, json=request_payload, timeout=30)
        response.raise_for_status()
        response_data = response.json()
        
        # Extract the generated text
        if "candidates" in response_data and response_data["candidates"]:
            text_response = response_data["candidates"][0]["content"]["parts"][0]["text"]
            
            # Try to parse JSON from the response
            json_match = re.search(r'\{.*\}', text_response, re.DOTALL)
            if json_match:
                language_data = json.loads(json_match.group())
                return language_data
            else:
                # Fallback if JSON parsing fails
                return {
                    "primary_language": "Unknown (parse error)",
                    "similar_languages": ["Unknown", "Unknown", "Unknown", "Unknown"],
                    "confidence": "Low"
                }
        else:
            return {
                "primary_language": "Unknown (no response)",
                "similar_languages": ["Unknown", "Unknown", "Unknown", "Unknown"],
                "confidence": "Low"
            }
            
    except Exception as e:
        print(f"Error detecting language with Gemini: {e}")
        return {
            "primary_language": "Unknown (error)",
            "similar_languages": ["Unknown", "Unknown", "Unknown", "Unknown"],
            "confidence": "Low"
        }

def read_code_file(filename):
    """Read the content of a code file"""
    try:
        with open(filename, 'r', encoding='utf-8') as file:
            return file.read()
    except Exception as e:
        print(f"Error reading file: {e}")
        return None

def generate_analysis_prompt(code, filename, language_info):
    """Generate a comprehensive prompt for code analysis"""
    
    # Concise system instruction
    system_instruction = """
    You are an expert code analysis assistant. Apply code reading strategies developed by 
    Felienne Hermans and Marit van Dijk to help developers understand unfamiliar code.
    """
    
    # Main analysis prompt with all detailed instructions
    prompt = f"""
    Analyze the following code from file: {filename}
    
    Detected Language: {language_info['primary_language']} (confidence: {language_info['confidence']})
    Similar Languages: {', '.join(language_info['similar_languages'])}
    
    CODE:
    ```
    {code}
    ```
    
    Please provide a comprehensive analysis using Felienne Hermans' code reading strategies:
    
    1. FIRST GLANCE: What stands out immediately about this code?
    
    2. KEY LINES: Identify the 5 most important lines and explain their significance
    
    3. SYNTACTIC ANALYSIS: What repeated patterns, variables, and structures do you notice?
    
    4. DOMAIN ANALYSIS: What domain-specific concepts and terms are present?
    
    5. CONTEXT ANALYSIS: How might this code fit into a larger system?
    
    6. STRUCTURAL ANALYSIS: What architectural patterns or dependencies are visible?
    
    7. COGNITIVE REFACTORING: How could this code be mentally restructured for better understanding?
    
    8. ACTIONABLE READING STRATEGIES: Specific techniques for understanding this codebase
    
    9. IDIOMATIC SUGGESTIONS: 
       - How could this code be made more idiomatic for {language_info['primary_language']}?
       - Provide specific examples of idiomatic improvements with code snippets
       - Highlight any anti-patterns or non-idiomatic constructs
    
    10. POTENTIAL CHALLENGES: 
        - What might be difficult to understand about this code?
        - What questions would you ask the original author?
    
    11. ADDITIONAL INSIGHTS:
        - Any notable code smells or best practices violations
        - Potential areas for improvement or refactoring
        - Security considerations if apparent
    """
    
    return system_instruction, prompt

def call_gemini_api(system_instruction, prompt):
    """Call the Gemini API with the provided instructions and prompt"""
    
    headers = {
        "x-goog-api-key": API_KEY,
        "Content-Type": "application/json"
    }
    
    request_payload = {
        "system_instruction": {
            "parts": [{"text": system_instruction}]
        },
        "contents": [
            {
                "parts": [{"text": prompt}]
            }
        ],
        "generationConfig": {
            "temperature": 0.3,  # Lower temperature for more focused analysis
            "topP": 0.8,
            "topK": 64,
            "thinkingConfig": {
                "thinkingBudget": -1,  # Use maximum thinking budget
                "includeThoughts": True  # Include reasoning process
            }
        },
        "tools": [
            {
                "google_search": {}  # Enable search grounding
            }
        ],
        "safetySettings": [
            {
                "category": "HARM_CATEGORY_HARASSMENT",
                "threshold": "BLOCK_MEDIUM_AND_ABOVE"
            },
            {
                "category": "HARM_CATEGORY_HATE_SPEECH",
                "threshold": "BLOCK_MEDIUM_AND_ABOVE"
            }
        ]
    }
    
    try:
        print("Sending analysis request to Gemini...")
        response = requests.post(GEMINI_API_ENDPOINT, headers=headers, json=request_payload, timeout=120)
        response.raise_for_status()
        return response.json()
    except Exception as e:
        print(f"Error calling Gemini API: {e}")
        return None

def extract_search_queries(response_data):
    """Extract search queries from the response if they exist"""
    if not response_data or "candidates" not in response_data or not response_data["candidates"]:
        return []
    
    candidate = response_data["candidates"][0]
    if "groundingMetadata" in candidate and "webSearchQueries" in candidate["groundingMetadata"]:
        return candidate["groundingMetadata"]["webSearchQueries"]
    
    return []

def extract_generated_text(response_data):
    """Extract all generated text from the response"""
    if not response_data or "candidates" not in response_data or not response_data["candidates"]:
        return ""
    
    generated_text = ""
    candidate = response_data["candidates"][0]
    
    # Extract all text parts from the response
    if "content" in candidate and "parts" in candidate["content"]:
        for part in candidate["content"]["parts"]:
            if "text" in part:
                generated_text += part["text"] + "\n\n"
    
    return generated_text

def save_analysis_report(filename, code, response_data, language_info):
    """Save the analysis report to a markdown file"""
    
    # Generate output filename
    base_name = os.path.splitext(os.path.basename(filename))[0]
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    output_filename = f"{base_name}_analysis_{timestamp}.md"
    output_path = os.path.join(analysis_folder, output_filename)
    
    # Extract generated text and search queries
    generated_text = extract_generated_text(response_data)
    search_queries = extract_search_queries(response_data)
    
    # Create the markdown report
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(f"# Code Analysis Report: {filename}\n\n")
        f.write(f"**Analysis Date:** {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        
        f.write("## Language Identification\n")
        f.write(f"- **Primary Language:** {language_info['primary_language']}\n")
        f.write(f"- **Confidence:** {language_info['confidence']}\n")
        f.write(f"- **Similar Languages:** {', '.join(language_info['similar_languages'])}\n\n")
        
        f.write("## Code Preview\n")
        f.write("```\n")
        # Show first 100 lines to avoid overly long reports
        lines = code.split('\n')
        for i, line in enumerate(lines[:100]):
            f.write(f"{i+1:3d} | {line}\n")
        if len(lines) > 100:
            f.write(f"... (showing first 100 of {len(lines)} lines)\n")
        f.write("```\n\n")
        
        f.write("## Analysis Results\n")
        f.write(generated_text)
        
        # Add search queries section if any were used
        if search_queries:
            f.write("## Search Queries Used\n")
            f.write("The following search queries were used to ground the analysis:\n\n")
            for query in search_queries:
                f.write(f"- {query}\n")
            f.write("\n")
        
        f.write("---\n\n")
        f.write("*This analysis was generated using strategies from Felienne Hermans and Marit van Dijk.*\n")
        f.write("*Language detection was performed by Gemini 2.5 Flash with search grounding and thinking enabled.*\n")
    
    return output_path

def main():
    """Main function to run the code reading assistant"""
    
    if not API_KEY:
        print("Error: GEMINI_API_KEY environment variable not set.")
        print("Please set your API key and try again.")
        return
    
    # Get filename from user
    filename = input("Enter the source code filename: ").strip()
    
    if not os.path.exists(filename):
        print(f"Error: File '{filename}' not found.")
        return
    
    # Read code file
    code = read_code_file(filename)
    if code is None:
        return
    
    # Detect language using Gemini
    language_info = detect_language_with_gemini(code, filename)
    print(f"Detected language: {language_info['primary_language']} ({language_info['confidence']} confidence)")
    print(f"Similar languages: {', '.join(language_info['similar_languages'])}")
    
    # Generate analysis prompt
    system_instruction, prompt = generate_analysis_prompt(code, filename, language_info)
    
    # Call Gemini API for analysis with thinking and search grounding enabled
    response_data = call_gemini_api(system_instruction, prompt)
    if not response_data:
        print("Failed to get analysis from Gemini.")
        return
    
    # Save report
    report_path = save_analysis_report(filename, code, response_data, language_info)
    print(f"Analysis complete! Report saved to: {report_path}")
    
    # Display search queries if any were used
    search_queries = extract_search_queries(response_data)
    if search_queries:
        print("\nSearch queries used for grounding:")
        for query in search_queries:
            print(f"- {query}")

if __name__ == "__main__":
    main()