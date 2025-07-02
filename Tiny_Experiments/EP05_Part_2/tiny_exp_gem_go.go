package main

import (
	"bufio"         // bufio package provides buffered I/O, useful for efficient reading of input.
	"bytes"         // bytes package implements functions for the manipulation of byte slices. Used here to create a buffer for HTTP request bodies.
	"encoding/json" // encoding/json package implements encoding and decoding of JSON as defined in RFC 7159. Essential for API communication.
	"fmt"           // fmt package implements formatted I/O (input/output) functions, similar to C's printf and scanf.
	"io"            // io package provides fundamental interfaces for I/O primitives. io.ReadAll is used to read the entire body of an HTTP response.
	"net/http"      // net/http package provides HTTP client and server implementations. Used for making API requests.
	"os"            // os package provides a platform-independent interface to operating system functionality, like accessing environment variables or standard input/output.
	"strconv"       // strconv package implements conversions to and from string representations of basic data types, such as converting strings to integers.
	"strings"       // strings package implements simple functions to manipulate UTF-8 encoded strings. Used for trimming whitespace.
	"time"          // time package provides functionality for measuring and displaying time, used here for setting HTTP client timeouts.
)

// defaultGeminiAPIKey is a placeholder for the Gemini API key.
// It's highly recommended to load the API key from environment variables for security reasons
// rather than hardcoding it directly in the source code.
const defaultGeminiAPIKey = "YOUR_GEMINI_API_KEY"

// geminiEndpoint defines the base URL for the Gemini API.
// This specific endpoint targets the 'gemini-2.5-flash' model for content generation.
const geminiEndpoint = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key="

// scanner is a global variable initialized to read from standard input (os.Stdin).
// Using a global scanner is more efficient as it avoids creating a new scanner
// for every input operation, especially in a loop.
var scanner *bufio.Scanner

// actualGeminiAPIKey stores the Gemini API key that the program will use.
// This variable is populated during the program's initialization,
// prioritizing the environment variable over the hardcoded default.
var actualGeminiAPIKey string

// GeminiRequest defines the structure for the JSON request body sent to the Gemini API.
//
// The 'Contents' field holds the input parts (e.g., text prompt) for the model.
// The 'GenerationConfig' is optional and can be used to specify parameters
// like response format (e.g., JSON), but for these experiments, it's kept simple
// for text generation.
type GeminiRequest struct {
	Contents []struct {
		Parts []struct {
			Text string `json:"text"`
		} `json:"parts"`
	} `json:"contents"`
	GenerationConfig struct {
		ResponseMimeType string `json:"responseMimeType,omitempty"`
	} `json:"generationConfig,omitempty"`
}

// GeminiResponse defines the structure for the JSON response body received from the Gemini API.
//
// The 'Candidates' field contains the model's generated responses.
// Each candidate typically has 'Content' with 'Parts' that include the generated 'Text'.
// An 'Error' field is included to capture any error details returned by the API,
// making error handling more robust.
type GeminiResponse struct {
	Candidates []struct {
		Content struct {
			Parts []struct {
				Text string `json:"text"`
			} `json:"parts"`
		} `json:"content"`
	} `json:"candidates"`
	Error struct {
		Code    int    `json:"code"`
		Message string `json:"message"`
		Status  string `json:"status"`
	} `json:"error"`
}

// main is the entry point of the Go application.
// It initializes the input scanner, attempts to retrieve the API key,
// and then enters a loop to display a menu and execute user-selected experiments.
func main() {
	// Initialize the global scanner to read from standard input (keyboard).
	scanner = bufio.NewScanner(os.Stdin)

	// Attempt to load the Gemini API key from the environment variable named "GEMINI_API_KEY".
	// This is the recommended practice for handling sensitive credentials.
	actualGeminiAPIKey = os.Getenv("GEMINI_API_KEY")
	if actualGeminiAPIKey == "" {
		// If the environment variable is not set, print a warning and fall back to the default hardcoded key.
		fmt.Println("\nWARNING: 'GEMINI_API_KEY' environment variable not found.")
		fmt.Println("Using hardcoded default API key. Please replace 'YOUR_GEMINI_API_KEY' or set the environment variable.")
		actualGeminiAPIKey = defaultGeminiAPIKey
		pressEnterToContinue() // Pause to allow the user to read the warning.
	}

	// Double-check if the API key being used is still the default placeholder.
	// This provides an additional warning if the user hasn't replaced the key.
	if actualGeminiAPIKey == defaultGeminiAPIKey {
		fmt.Println("\nWARNING: Gemini API Key is still the default placeholder. Please set 'GEMINI_API_KEY' environment variable or replace the hardcoded value.")
		fmt.Println("Some experiments may not function correctly without a valid API key.")
		pressEnterToContinue() // Pause to allow the user to read the warning.
	}

	// The main program loop. It continuously displays the menu and processes user input
	// until the user chooses to exit.
	for {
		displayMenu() // Show the interactive menu options to the user.

		// Prompt the user for their choice and read the input.
		inputStr := readInput("Enter your choice (0-10): ")

		// Attempt to convert the user's input string into an integer.
		choice, err := strconv.Atoi(inputStr)
		if err != nil {
			// If the input is not a valid number, print an error message.
			fmt.Println("Error: Invalid input. Please enter a number.")
			pressEnterToContinue() // Pause so the user can see the error.
			continue               // Skip to the next iteration of the loop (redisplay the menu).
		}

		// Use a switch statement to perform actions based on the user's integer choice.
		switch choice {
		case 0:
			// Option 0: Exit the program.
			fmt.Println("\nExiting program. Goodbye!")
			return // Terminates the main function, ending the application.
		case 1:
			experiment1() // Call the function for Experiment 1.
		case 2:
			experiment2() // Call the function for Experiment 2.
		case 3:
			experiment3() // Call the function for Experiment 3.
		case 4:
			experiment4() // Call the function for Experiment 4.
		case 5:
			experiment5() // Call the function for Experiment 5.
		case 6:
			experiment6() // Call the function for Experiment 6.
		case 7:
			experiment7() // Call the function for Experiment 7.
		case 8:
			experiment8() // Call the function for Experiment 8.
		case 9:
			experiment9() // Call the function for Experiment 9.
		case 10:
			experiment10() // Call the function for Experiment 10.
		default:
			// Handle any choice that is outside the valid range (0-10).
			fmt.Println("Error: Invalid choice. Please enter a number between 0 and 10.")
		}

		// After an experiment runs or an invalid choice is handled, pause execution
		// before looping back to display the menu again.
		pressEnterToContinue()
	}
}

// displayMenu prints the main menu options to the console.
// It provides a clear list of available Gemini API experiments.
func displayMenu() {
	fmt.Println("\n--- Interactive Gemini API Experiment Dispatcher ---")
	fmt.Println("Select an option to run an experiment:")
	fmt.Println("----------------------------------------------------")
	fmt.Println(" 1. Basic Text Generation: Fun Fact about Go")
	fmt.Println(" 2. Creative Writing Prompt: Whimsical Cloud Story")
	fmt.Println(" 3. Summarization: Sample Text Summary")
	fmt.Println(" 4. Translation: English to French")
	fmt.Println(" 5. Code Snippet Generation: Go Factorial Function")
	fmt.Println(" 6. Question Answering: Capital of Canada")
	fmt.Println(" 7. Brainstorming Ideas: Productivity App Ideas")
	fmt.Println(" 8. Sentiment Analysis: Sample Sentence")
	fmt.Println(" 9. Recipe Suggestion: Chicken & Broccoli")
	fmt.Println("10. Role-Playing: Grumpy Wizard on Elves")
	fmt.Println(" 0. Exit Program")
	fmt.Println("----------------------------------------------------")
}

// readInput is a helper function that displays a prompt to the user
// and reads a single line of input from the console.
// It trims any leading or trailing whitespace from the input string.
func readInput(prompt string) string {
	fmt.Print(prompt)                        // Display the provided prompt to the user.
	scanner.Scan()                           // Read the next line of input from the scanner.
	return strings.TrimSpace(scanner.Text()) // Return the read text after removing leading/trailing whitespace.
}

// pressEnterToContinue pauses the program execution and waits for the user
// to press the Enter key before proceeding. This is useful for allowing users
// to read output before the screen clears or the menu reappears.
func pressEnterToContinue() {
	fmt.Println("\nPress Enter to continue...")
	scanner.Scan() // Read the line (the Enter key press) and discard its content.
}

// callGeminiAPI sends a text prompt to the Gemini API and retrieves its generated response.
// It constructs the HTTP request, handles JSON encoding/decoding, sends the request,
// processes the response, and extracts the generated text or any API errors.
func callGeminiAPI(prompt string) (string, error) {
	// Construct the complete API endpoint URL by appending the actual API key.
	fullEndpoint := geminiEndpoint + actualGeminiAPIKey

	// Create the request body in the `GeminiRequest` format, embedding the user's prompt.
	requestBody := GeminiRequest{
		Contents: []struct {
			Parts []struct {
				Text string `json:"text"`
			} `json:"parts"`
		}{
			{
				Parts: []struct {
					Text string `json:"text"`
				}{
					{Text: prompt}, // The actual text prompt sent to the Gemini model.
				},
			},
		},
	}

	// Marshal (encode) the Go `requestBody` struct into a JSON byte slice.
	jsonPayload, err := json.Marshal(requestBody)
	if err != nil {
		return "", fmt.Errorf("failed to marshal request body: %w", err)
	}

	// Create a new HTTP client with a specified timeout to prevent indefinite waiting.
	client := &http.Client{Timeout: 30 * time.Second}

	// Create a new HTTP POST request.
	// The request body is provided as a `bytes.Buffer` containing the JSON payload.
	req, err := http.NewRequest("POST", fullEndpoint, bytes.NewBuffer(jsonPayload))
	if err != nil {
		return "", fmt.Errorf("failed to create HTTP request: %w", err)
	}

	// Set the "Content-Type" header to "application/json" to inform the API
	// that the request body is in JSON format.
	req.Header.Set("Content-Type", "application/json")

	// Send the HTTP request to the Gemini API.
	resp, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("failed to send HTTP request: %w", err)
	}
	// Defer closing the response body until the surrounding function returns.
	// This ensures resources are released properly.
	defer resp.Body.Close()

	// Read the entire response body from the API.
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("failed to read response body: %w", err)
	}

	// Check the HTTP status code. If it's not 200 OK, it indicates an API error.
	if resp.StatusCode != http.StatusOK {
		var errorResponse GeminiResponse
		// Attempt to unmarshal the error response to get more specific details.
		json.Unmarshal(body, &errorResponse)
		if errorResponse.Error.Message != "" {
			// If an error message is available from the API, include it in our error.
			return "", fmt.Errorf("API returned error status %d: %s (Code: %d, Status: %s)",
				resp.StatusCode, errorResponse.Error.Message, errorResponse.Error.Code, errorResponse.Error.Status)
		}
		// If no specific error message from the API, return a generic error with the status code and raw body.
		return "", fmt.Errorf("API returned non-OK status: %d - %s", resp.StatusCode, string(body))
	}

	// Unmarshal (decode) the JSON response body into the `GeminiResponse` struct.
	var geminiResp GeminiResponse
	err = json.Unmarshal(body, &geminiResp)
	if err != nil {
		return "", fmt.Errorf("failed to unmarshal response body: %w", err)
	}

	// Check if the response contains any candidates (generated text) and parts.
	// If so, return the text from the first part of the first candidate.
	if len(geminiResp.Candidates) > 0 && len(geminiResp.Candidates[0].Content.Parts) > 0 {
		return geminiResp.Candidates[0].Content.Parts[0].Text, nil
	}

	// If no text was generated or found in the response, return a default message.
	return "No text generated.", nil
}

// --- Experiment Functions ---
// Each experiment function demonstrates a specific use case of the Gemini API
// by crafting a unique prompt and displaying the model's response.
// A "Tiny Habit" suggestion is included for each, promoting practical application.

// experiment1: Demonstrates basic text generation by asking Gemini for a fun fact about Go programming.
func experiment1() {
	fmt.Println("\n--- Running Experiment 1: Basic Text Generation ---")
	fmt.Println("Prompting Gemini for a fun fact about Go programming.")
	prompt := "Tell me a fun fact about Go programming."
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Daily 5-minute Go fact check. Before starting coding, generate and read one fun fact about Go.")
		return
	}
	fmt.Printf("\nGemini's Fun Fact:\n%s\n", response)
	fmt.Println("\nTiny Habit: Daily 5-minute Go fact check. Before starting coding, generate and read one fun fact about Go.")
	fmt.Println("--- Experiment 1 Finished ---\n")
}

// experiment2: Explores creative writing by prompting Gemini to generate a whimsical story about a cloud.
func experiment2() {
	fmt.Println("\n--- Running Experiment 2: Creative Writing Prompt ---")
	fmt.Println("Prompting Gemini to write a short, whimsical story about a cloud.")
	prompt := "Write a short, whimsical story about a cloud who lost its shape and went on an adventure to find it."
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Creative Spark. Every evening, generate a short story prompt and jot down 3 ideas inspired by it.")
		return
	}
	fmt.Printf("\nGemini's Story:\n%s\n", response)
	fmt.Println("\nTiny Habit: Creative Spark. Every evening, generate a short story prompt and jot down 3 ideas inspired by it.")
	fmt.Println("--- Experiment 2 Finished ---\n")
}

// experiment3: Showcases summarization capabilities by asking Gemini to summarize a given text into a concise sentence.
func experiment3() {
	fmt.Println("\n--- Running Experiment 3: Summarization ---")
	fmt.Println("Prompting Gemini to summarize a sample paragraph.")
	sampleText := `The quick brown fox jumps over the lazy dog. This sentence is famous for containing every letter of the English alphabet, making it a pangram. It is often used to test typewriters and computer keyboards. Its simplicity and completeness make it an excellent tool for demonstrating fonts and character sets. Despite its brevity, it has a long history in typography and computing.`
	prompt := fmt.Sprintf("Summarize the following text in one concise sentence: \"%s\"", sampleText)
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Concise Communication. Once a week, summarize a complex article or document using Gemini, then compare its summary to your own.")
		return
	}
	fmt.Printf("\nOriginal Text:\n%s\n", sampleText)
	fmt.Printf("\nGemini's Summary:\n%s\n", response)
	fmt.Println("\nTiny Habit: Concise Communication. Once a week, summarize a complex article or document using Gemini, then compare its summary to your own.")
	fmt.Println("--- Experiment 3 Finished ---\n")
}

// experiment4: Demonstrates translation by requesting Gemini to translate an English sentence to French.
func experiment4() {
	fmt.Println("\n--- Running Experiment 4: Translation ---")
	fmt.Println("Prompting Gemini to translate a sentence from English to French.")
	englishSentence := "Hello, how are you today? I hope you are having a wonderful time."
	prompt := fmt.Sprintf("Translate the following English sentence to French: \"%s\"", englishSentence)
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Language Learner. Translate one common phrase daily using Gemini, then try to use it in conversation or thought.")
		return
	}
	fmt.Printf("\nOriginal English:\n%s\n", englishSentence)
	fmt.Printf("\nGemini's French Translation:\n%s\n", response)
	fmt.Println("\nTiny Habit: Language Learner. Translate one common phrase daily using Gemini, then try to use it in conversation or thought.")
	fmt.Println("--- Experiment 4 Finished ---\n")
}

// experiment5: Highlights code generation capabilities by asking Gemini to create a Go factorial function.
func experiment5() {
	fmt.Println("\n--- Running Experiment 5: Code Snippet Generation ---")
	fmt.Println("Prompting Gemini to generate a simple Go function for factorial calculation.")
	prompt := "Generate a simple Go function to calculate the factorial of a non-negative integer. Include comments."
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Code Review Buddy. Before writing a new utility function, ask Gemini for a basic implementation and compare it to your approach.")
		return
	}
	fmt.Printf("\nGemini's Go Factorial Function:\n%s\n", response)
	fmt.Println("\nTiny Habit: Code Review Buddy. Before writing a new utility function, ask Gemini for a basic implementation and compare it to your approach.")
	fmt.Println("--- Experiment 5 Finished ---\n")
}

// experiment6: Tests question answering ability by querying Gemini for the capital of Canada.
func experiment6() {
	fmt.Println("\n--- Running Experiment 6: Question Answering ---")
	fmt.Println("Prompting Gemini for the capital of Canada.")
	prompt := "What is the capital of Canada?"
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Curiosity Cultivator. When a random question pops into your head, use Gemini to find the answer immediately.")
		return
	}
	fmt.Printf("\nGemini's Answer:\n%s\n", response)
	fmt.Println("\nTiny Habit: Curiosity Cultivator. When a random question pops into your head, use Gemini to find the answer immediately.")
	fmt.Println("--- Experiment 6 Finished ---\n")
}

// experiment7: Explores brainstorming by requesting Gemini to suggest ideas for a mobile productivity app.
func experiment7() {
	fmt.Println("\n--- Running Experiment 7: Brainstorming Ideas ---")
	fmt.Println("Prompting Gemini for 5 ideas for a mobile productivity app.")
	prompt := "Suggest 5 innovative ideas for a mobile app that helps with productivity."
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Idea Generator. Once a week, dedicate 10 minutes to brainstorming new project ideas using Gemini as a co-pilot.")
		return
	}
	fmt.Printf("\nGemini's App Ideas:\n%s\n", response)
	fmt.Println("\nTiny Habit: Idea Generator. Once a week, dedicate 10 minutes to brainstorming new project ideas using Gemini as a co-pilot.")
	fmt.Println("--- Experiment 7 Finished ---\n")
}

// experiment8: Demonstrates sentiment analysis by asking Gemini to determine the sentiment of a given sentence.
func experiment8() {
	fmt.Println("\n--- Running Experiment 8: Sentiment Analysis ---")
	fmt.Println("Prompting Gemini to determine the sentiment of a sentence.")
	sampleSentence := "The customer service was absolutely dreadful, and the product broke on the first day."
	prompt := fmt.Sprintf("Analyze the sentiment of the following sentence (positive, negative, or neutral): \"%s\"", sampleSentence)
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Empathy Builder. Analyze the sentiment of a few customer feedback snippets daily to better understand user emotions.")
		return
	}
	fmt.Printf("\nOriginal Sentence:\n%s\n", sampleSentence)
	fmt.Printf("\nGemini's Sentiment Analysis:\n%s\n", response)
	fmt.Println("\nTiny Habit: Empathy Builder. Analyze the sentiment of a few customer feedback snippets daily to better understand user emotions.")
	fmt.Println("--- Experiment 8 Finished ---\n")
}

// experiment9: Provides a practical application by asking Gemini for a simple dinner recipe.
func experiment9() {
	fmt.Println("\n--- Running Experiment 9: Recipe Suggestion ---")
	fmt.Println("Prompting Gemini for a simple dinner recipe using chicken and broccoli.")
	prompt := "Suggest a simple dinner recipe using chicken and broccoli. Include ingredients and basic steps."
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Meal Prep Assistant. Before grocery shopping, ask Gemini for a recipe based on ingredients you have, reducing food waste.")
		return
	}
	fmt.Printf("\nGemini's Recipe Suggestion:\n%s\n", response)
	fmt.Println("\nTiny Habit: Meal Prep Assistant. Before grocery shopping, ask Gemini for a recipe based on ingredients you have, reducing food waste.")
	fmt.Println("--- Experiment 9 Finished ---\n")
}

// experiment10: Showcases role-playing capabilities by having Gemini act as a grumpy wizard discussing elves.
func experiment10() {
	fmt.Println("\n--- Running Experiment 10: Role-Playing ---")
	fmt.Println("Prompting Gemini to act as a grumpy old wizard and talk about elves.")
	prompt := "Act as a grumpy old wizard. Tell me why I shouldn't trust elves. Be concise and a bit cynical."
	response, err := callGeminiAPI(prompt)
	if err != nil {
		fmt.Printf("Error calling Gemini API: %v\n", err)
		fmt.Println("Tiny Habit: Perspective Shift. Once a month, try to solve a problem by asking Gemini to role-play a different persona.")
		return
	}
	fmt.Printf("\nGemini (as Grumpy Wizard):\n%s\n", response)
	fmt.Println("\nTiny Habit: Perspective Shift. Once a month, try to solve a problem by asking Gemini to role-play a different persona (e.g., a minimalist, a CEO, a child).")
	fmt.Println("--- Experiment 10 Finished ---\n")
}
