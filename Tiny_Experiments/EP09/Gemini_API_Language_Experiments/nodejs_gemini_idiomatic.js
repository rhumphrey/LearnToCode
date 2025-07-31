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
// It's a good practice to centralize configuration. This makes it easier to manage and update.
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
 * Custom error class for API-related errors. This allows for more specific error handling.
 */
class ApiError extends Error {
    /**
     * @param {string} message The error message.
     * @param {number} status The HTTP status code.
     * @param {string} statusText The HTTP status text.
     * @param {string} body The response body.
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
 * Exits the process if a required variable is missing.
 */
function validateEnvironment() {
    if (!config.apiKey) {
        console.error("Error: GEMINI_API_KEY environment variable not set.");
        process.exit(1);
    }
}

/**
 * Helper function to pause execution and wait for the user to press Enter.
 * This is a cleaner implementation that properly manages the readline interface.
 * @returns {Promise<void>}
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
 * It centralizes fetch logic, error handling, and headers.
 * @param {string} url The URL to fetch.
 * @param {object} options The fetch options.
 * @returns {Promise<any>} A promise that resolves with the JSON response.
 * @throws {ApiError} If the API response is not ok.
 */
async function fetchFromApi(url, options = {}) {
    const defaultHeaders = {
        "Content-Type": "application/json",
        "x-goog-api-key": config.apiKey,
    };

    const fetchOptions = {
        ...options,
        headers: {
            ...defaultHeaders,
            ...options.headers,
        },
    };

    const response = await fetch(url, fetchOptions);

    if (!response.ok) {
        const errorBody = await response.text();
        throw new ApiError(
            `HTTP error!`,
            response.status,
            response.statusText,
            errorBody
        );
    }
    return response.json();
}


/**
 * Fetches and displays the model's metadata.
 * The function is now more focused on its primary responsibility.
 */
async function getModelData() {
    console.log("\n--- Default Model Data Before Any Changes ---");
    try {
        const url = `${config.modelEndpoint}?key=${config.apiKey}`;
        const modelData = await fetchFromApi(url);

        for (const [key, value] of Object.entries(modelData)) {
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
 * Generates content using the Gemini API based on the configuration.
 */
async function requestResponse() {
    const { request: requestConfig } = config;

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

    if (requestConfig.enable_grounding) {
        requestPayload.tools = [{ "google_search": {} }];
    }

    console.log("Sending Request...");
    try {
        const responseData = await fetchFromApi(`${config.apiEndpoint}?key=${config.apiKey}`, {
            method: 'POST',
            body: JSON.stringify(requestPayload)
        });
        displayResponse(responseData);
    } catch (error) {
        console.error("Error sending request:", error.message);
        if (error instanceof ApiError) {
            console.error(`Status: ${error.status} ${error.statusText}`);
            console.error(`Response Body: ${error.body}`);
        }
    } finally {
        printSummary();
    }
}

/**
 * Displays the formatted response from the Gemini API.
 * Separating display logic improves readability and separation of concerns.
 * @param {object} responseData The response data from the API.
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

        if (candidate.groundingMetadata) {
            displayGroundingInfo(candidate.groundingMetadata);
        }
    } else {
        console.log("No candidates found in the response.");
        console.log("Full Response:", JSON.stringify(responseData, null, 2));
    }
}

/**
 * Displays the grounding information from the API response.
 * @param {object} groundingMetadata The grounding metadata.
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
 * Prints a summary of the request configuration.
 */
function printSummary() {
    const { request: requestConfig } = config;
    console.log("\nSummary of values set for this request");
    console.log("---------------------------------------");
    for (const [key, value] of Object.entries(requestConfig)) {
        // For text and instruction, trim them for a cleaner display
        const displayValue = (key === 'text' || key === 'instruction') ? String(value).trim() : value;
        console.log(`${key}: ${displayValue}`);
    }
    console.log("---------------------------------------");
}

/**
 * Main function to run the script.
 * Using a main function is a good practice for organizing the execution flow.
 */
async function main() {
    validateEnvironment();
    await getModelData();
    await pressAnyKey();
    await requestResponse();
}

// The script execution starts here
main().catch(error => {
    console.error("An unexpected error occurred in the main execution block:", error);
    process.exit(1);
});