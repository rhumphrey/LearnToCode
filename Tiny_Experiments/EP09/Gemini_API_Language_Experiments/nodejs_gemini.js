// For reading user input
const readline = require('readline');

/**
 * It is highly recommended to set your API key as an environment variable
 * Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
 * Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
 */
const API_KEY = process.env.GEMINI_API_KEY;

const GEMINI_API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent";
const GEMINI_MODEL_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash";


/**
 * Helper function to pause execution and wait for the user to press Enter.
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
 * Fetches and displays the model's metadata.
 */
async function getModelData() {
    if (!API_KEY) {
        console.error("Error: GEMINI_API_KEY environment variable not set.");
        process.exit(1);
    }
    
    const url = `${GEMINI_MODEL_ENDPOINT}?key=${API_KEY}`;

    try {
        const response = await fetch(url);
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status} ${response.statusText}`);
        }
        const modelData = await response.json();

        console.log("\n--- Default Model Data Before Any Changes ---");
        for (const [key, value] of Object.entries(modelData)) {
            // Printing nested objects can be messy, so we'll stringify them
            const displayValue = typeof value === 'object' ? JSON.stringify(value, null, 2) : value;
            console.log(`${key}: ${displayValue}`);
        }
    } catch (error) {
        console.error("Error fetching model data:", error.message);
    }
}


/**
 * Generates content using the Gemini API based on a set of configurations.
 */
async function requestResponse() {
    if (!API_KEY) {
        console.error("Error: GEMINI_API_KEY environment variable not set.");
        process.exit(1);
    }

    // --- Configuration ---
    const text = "Explain the concept of LLM embeddings in simple terms.";
    const instruction = "You are a helpful AI assistant with a flourishing mindset";
    const temperature = 1;
    const topP = 0.95;
    const topK = 64;
    const thinking_budget = -1; // -1 means no budget
    const include_thoughts = false;
    const enable_grounding = false; // Set to true to test grounding

    // --- Request Payload ---
    const headers = {
        "x-goog-api-key": API_KEY,
        "Content-Type": "application/json"
    };

    const requestPayload = {
        "system_instruction": {
            "parts": [{ "text": instruction }]
        },
        "contents": [
            { "parts": [{ "text": text }] }
        ],
        "generationConfig": {
            "temperature": temperature,
            "topP": topP,
            "topK": topK,
            "thinkingConfig": {
                "thinkingBudget": thinking_budget,
                "includeThoughts": include_thoughts
            }
        },
    };

    // Conditionally enable grounding by adding the tools object
    if (enable_grounding) {
        requestPayload.tools = [{ "google_search": {} }];
    }

    // --- Send Request ---
    console.log("Sending Request...");
    try {
        const response = await fetch(`${GEMINI_API_ENDPOINT}?key=${API_KEY}`, {
            method: 'POST',
            headers: headers,
            body: JSON.stringify(requestPayload)
        });

        if (!response.ok) {
             const errorBody = await response.text();
             throw new Error(`HTTP error! status: ${response.status} ${response.statusText}\nBody: ${errorBody}`);
        }

        const responseData = await response.json();

        // --- Process Response ---
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

            // Check for grounding metadata
            if (candidate.groundingMetadata) {
                const { webSearchQueries, groundingChunks } = candidate.groundingMetadata;
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
        } else {
            console.log("No candidates found in the response.");
            console.log("Full Response:", JSON.stringify(responseData, null, 2));
        }

    } catch (error) {
        console.error("Error sending request:", error.message);
    } finally {
        // --- Print Summary ---
        console.log("\nSummary of values set for this request");
        console.log("---------------------------------------");
        console.log(`text: ${text.trim()}`);
        console.log(`instruction: ${instruction.trim()}`);
        console.log(`temperature: ${temperature}`);
        console.log(`topP: ${topP}`);
        console.log(`topK: ${topK}`);
        console.log(`thinking_budget: ${thinking_budget}`);
        console.log(`include_thoughts: ${include_thoughts}`);
        console.log(`enable_grounding: ${enable_grounding}`);
        console.log("---------------------------------------");
    }
}


/**
 * Main function to run the script steps in order.
 */
async function main() {
    await getModelData();
    await pressAnyKey();
    await requestResponse();
}

main();