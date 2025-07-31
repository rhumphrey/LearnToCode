/**
 * @fileoverview This script interacts with the Gemini API to fetch model data and generate content.
 * It demonstrates best practices for Node.js including modularity, configuration management,
 * error handling, and asynchronous programming.
 *
 * To use this script, ensure you have a GEMINI_API_KEY environment variable set.
 * For Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
 * For Windows: set GEMINI_API_KEY=YOUR_API_KEY
 */

const readline = require('readline');

// --- Configuration ---
/**
 * Centralized configuration object.
 * Storing settings in a single object makes the code cleaner and easier to manage.
 * @type {{
 *   apiKey: string | undefined,
 *   apiEndpoint: string,
 *   modelEndpoint: string,
 *   request: {
 *     text: string,
 *     instruction: string,
 *     temperature: number,
 *     topP: number,
 *     topK: number,
 *     thinking_budget: number,
 *     include_thoughts: boolean,
 *     enable_grounding: boolean
 *   }
 * }}
 */
const config = {
    apiKey: process.env.GEMINI_API_KEY,
    apiEndpoint: "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent",
    modelEndpoint: "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash",
    request: {
        text: "Explain the concept of LLM embeddings in simple terms.",
        instruction: "You are a helpful AI assistant with a flourishing mindset",
        temperature: 1,
        topP: 0.95,
        topK: 64,
        thinking_budget: -1, // -1 means no budget
        include_thoughts: false,
        enable_grounding: false, // Set to true to test grounding
    }
};

/**
 * Custom error class for API-related errors.
 * This allows for more specific and detailed error handling compared to generic Error objects.
 */
class ApiError extends Error {
    /**
     * Creates an instance of ApiError.
     * @param {string} message - The primary error message.
     * @param {number} status - The HTTP status code from the API response.
     * @param {string} statusText - The HTTP status text from the API response.
     * @param {string} body - The raw response body, which may contain more details.
     */
    constructor(message, status, statusText, body) {
        super(message);
        this.name = 'ApiError';
        this.status = status;
        this.statusText = statusText;
        this.body = body;
    }
}

/**
 * Validates that the necessary environment variables are set.
 * Exits the process with a non-zero status code if a required variable is missing.
 */
function validateEnvironment() {
    if (!config.apiKey) {
        console.error("Error: GEMINI_API_KEY environment variable not set.");
        process.exit(1); // Exit if the API key is not configured.
    }
}

/**
 * Pauses execution and waits for the user to press the Enter key.
 * @returns {Promise<void>} A promise that resolves when the user presses Enter.
 */
function pressAnyKey() {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    return new Promise(resolve => {
        rl.question('\nPress any key to continue\n', () => {
            rl.close();
            resolve();
        });
    });
}

/**
 * A reusable fetch wrapper for interacting with the Gemini API.
 * It centralizes fetch logic, error handling, and headers for all API calls.
 * @param {string} url - The URL to fetch.
 * @param {RequestInit} [options={}] - The options for the fetch request (e.g., method, body).
 * @returns {Promise<any>} A promise that resolves with the JSON response from the API.
 * @throws {ApiError} If the API response has a non-successful HTTP status code.
 */
async function fetchFromApi(url, options = {}) {
    const defaultHeaders = {
        "Content-Type": "application/json",
        "x-goog-api-key": config.apiKey,
    };

    // Merge default and provided headers
    const fetchOptions = {
        ...options,
        headers: {
            ...defaultHeaders,
            ...options.headers,
        },
    };

    const response = await fetch(url, fetchOptions);

    if (!response.ok) {
        // Read the error body as text to provide more context
        const errorBody = await response.text();
        throw new ApiError(
            `HTTP error!`,
            response.status,
            response.statusText,
            errorBody
        );
    }
    // If the response is successful, parse it as JSON
    return response.json();
}


/**
 * Fetches and displays the model's metadata.
 * This demonstrates a simple GET request to retrieve model information.
 */
async function getModelData() {
    console.log("\n--- Default Model Data Before Any Changes ---");
    try {
        const url = `${config.modelEndpoint}?key=${config.apiKey}`;
        const modelData = await fetchFromApi(url);

        // Iterate over the model data and print key-value pairs
        for (const [key, value] of Object.entries(modelData)) {
            // Stringify objects for cleaner console output
            const displayValue = typeof value === 'object' ? JSON.stringify(value, null, 2) : value;
            console.log(`${key}: ${displayValue}`);
        }
    } catch (error) {
        console.error("Error fetching model data:", error.message);
        if (error instanceof ApiError) {
            console.error(`Status: ${error.status} ${error.statusText}`);
            console.error(`Response Body: ${error.body}`);
        }
    }
}

/**
 * Constructs the request payload and generates content using the Gemini API.
 */
async function requestResponse() {
    const { request: requestConfig } = config;

    // Construct the payload from the centralized config
    const requestPayload = {
        "system_instruction": {
            "parts": [{ "text": requestConfig.instruction }]
        },
        "contents": [
            { "parts": [{ "text": requestConfig.text }] }
        ],
        "generationConfig": {
            "temperature": requestConfig.temperature,
            "topP": requestConfig.topP,
            "topK": requestConfig.topK,
            "thinkingConfig": {
                "thinkingBudget": requestConfig.thinking_budget,
                "includeThoughts": requestConfig.include_thoughts
            }
        },
    };

    // Conditionally add grounding tools if enabled in the config
    if (requestConfig.enable_grounding) {
        requestPayload.tools = [{ "google_search": {} }];
    }

    console.log("Sending Request...");
    try {
        const url = `${config.apiEndpoint}?key=${config.apiKey}`;
        const responseData = await fetchFromApi(url, {
            method: 'POST',
            body: JSON.stringify(requestPayload)
        });
        // Pass the response to a dedicated display function
        displayResponse(responseData);
    } catch (error) {
        console.error("Error sending request:", error.message);
        if (error instanceof ApiError) {
            console.error(`Status: ${error.status} ${error.statusText}`);
            console.error(`Response Body: ${error.body}`);
        }
    } finally {
        // Always print a summary, even if the request fails
        printSummary();
    }
}

/**
 * Displays the formatted response from the Gemini API.
 * Separating display logic improves readability and follows the Single Responsibility Principle.
 * @param {object} responseData - The complete JSON response object from the API.
 */
function displayResponse(responseData) {
    console.log("\n--- Response Data ---");

    if (responseData.candidates && responseData.candidates.length > 0) {
        const candidate = responseData.candidates[0];
        const generatedParts = candidate.content.parts;

        console.log("Generated Text:\n");
        generatedParts.forEach(part => {
            if (part.text) {
                console.log(part.text);
                console.log("-".repeat(50));
            }
        });

        // Check for and display grounding metadata if it exists
        if (candidate.groundingMetadata) {
            displayGroundingInfo(candidate.groundingMetadata);
        }
    } else {
        console.log("No candidates found in the response.");
        console.log("Full Response:", JSON.stringify(responseData, null, 2));
    }
}

/**
 * Displays the grounding information (search queries and source links) from the API response.
 * @param {object} groundingMetadata - The groundingMetadata object from the API candidate.
 */
function displayGroundingInfo(groundingMetadata) {
    const { webSearchQueries, groundingChunks } = groundingMetadata;
    console.log("\n--- Google Grounding Info ---");

    if (webSearchQueries && webSearchQueries.length > 0) {
        console.log("Search Queries:");
        webSearchQueries.forEach(query => console.log(`  - ${query}`));
    }

    if (groundingChunks && groundingChunks.length > 0) {
        console.log("\nSource Links:");
        groundingChunks.forEach(chunk => {
            if (chunk.web && chunk.web.uri) {
                console.log(`  - ${chunk.web.uri}`);
            }
        });
    }
    console.log("-".repeat(50));
}


/**
 * Prints a summary of the request configuration values.
 * This helps in debugging and verifying the settings used for the API call.
 */
function printSummary() {
    const { request: requestConfig } = config;
    console.log("\nSummary of values set for this request");
    console.log("---------------------------------------");
    // Loop through the request config and print each key-value pair
    for (const [key, value] of Object.entries(requestConfig)) {
        const displayValue = (key === 'text' || key === 'instruction') ? String(value).trim() : value;
        console.log(`${key}: ${displayValue}`);
    }
    console.log("---------------------------------------");
}

/**
 * Main function to orchestrate the script's execution flow.
 * Using a `main` async function is a modern and clean way to structure a Node.js script.
 */
async function main() {
    validateEnvironment();
    await getModelData();
    await pressAnyKey();
    await requestResponse();
}

// Start the script execution and catch any top-level unhandled errors.
main().catch(error => {
    console.error("An unexpected error occurred in the main execution block:", error);
    process.exit(1);
});