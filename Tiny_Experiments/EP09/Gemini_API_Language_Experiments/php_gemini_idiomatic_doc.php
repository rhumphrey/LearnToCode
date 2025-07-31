<?php

/**
 * Custom exception class for handling errors specific to the Gemini API.
 * This class extends the base Exception class to provide more context
 * about API-related failures, including the HTTP status code and the
 * raw API response if available.
 */
class GeminiApiException extends Exception
{
    /**
     * @var array|null Stores the decoded API response body, if available.
     */
    protected ?array $apiResponse;

    /**
     * @var int Stores the HTTP status code returned by the API.
     */
    protected int $httpStatusCode;

    /**
     * Constructor for GeminiApiException.
     *
     * @param string $message The exception message.
     * @param int $httpStatusCode The HTTP status code (default: 0 if not an HTTP error).
     * @param array|null $apiResponse The decoded API response array (default: null).
     * @param Throwable|null $previous The previous throwable used for the exception chaining (default: null).
     */
    public function __construct(
        string $message,
        int $httpStatusCode = 0,
        ?array $apiResponse = null,
        ?Throwable $previous = null // Explicitly marked as nullable for PHP 8+ compatibility
    ) {
        parent::__construct($message, $httpStatusCode, $previous);
        $this->httpStatusCode = $httpStatusCode;
        $this->apiResponse = $apiResponse;
    }

    /**
     * Get the API response array.
     *
     * @return array|null The API response array or null if not available.
     */
    public function getApiResponse(): ?array
    {
        return $this->apiResponse;
    }

    /**
     * Get the HTTP status code.
     *
     * @return int The HTTP status code.
     */
    public function getHttpStatusCode(): int
    {
        return $this->httpStatusCode;
    }
}

/**
 * A client for interacting with the Google Gemini API.
 * This class provides methods to fetch model metadata and generate content
 * using the Gemini API, encapsulating the cURL requests and error handling.
 */
class GeminiApiClient
{
    /**
     * @var string The base URL for the Gemini API.
     */
    private const BASE_URL = "https://generativelanguage.googleapis.com/v1beta";

    /**
     * @var string The default Gemini model name to use if not specified.
     */
    private const DEFAULT_MODEL_NAME = "gemini-2.5-flash";

    /**
     * @var string The API key used for authentication with the Gemini API.
     */
    private string $apiKey;

    /**
     * @var bool Flag to enable or disable verbose cURL output for debugging.
     */
    private bool $debugCurl;

    /**
     * GeminiApiClient constructor.
     *
     * @param string $apiKey Your Gemini API key. It is highly recommended to retrieve this from environment variables.
     * @param bool $debugCurl Set to true to enable verbose cURL output for debugging purposes.
     * @throws InvalidArgumentException If the provided API key is empty.
     */
    public function __construct(string $apiKey, bool $debugCurl = false)
    {
        if (empty($apiKey)) {
            throw new InvalidArgumentException("API key cannot be empty.");
        }
        $this->apiKey = $apiKey;
        $this->debugCurl = $debugCurl;
    }

    /**
     * Internal helper method to make an HTTP request to the Gemini API.
     * This method handles common cURL options, request types (GET/POST),
     * header management (including API key), and robust error handling.
     *
     * @param string $endpoint The API endpoint path (e.g., "/models/gemini-2.5-flash:generateContent").
     * @param string $method The HTTP method to use ('GET' or 'POST').
     * @param array $payload The request body for POST requests, will be JSON encoded.
     * @param array $customHeaders Additional HTTP headers to send with the request.
     * @return array The decoded JSON response from the API.
     * @throws GeminiApiException If the request fails (cURL error, empty response, JSON decode error, or non-2xx HTTP status).
     */
    private function _makeRequest(string $endpoint, string $method, array $payload = [], array $customHeaders = []): array
    {
        $url = self::BASE_URL . $endpoint;
        $ch = curl_init();

        // Always include the API key in the x-goog-api-key header for authentication.
        $headersToSend = ["x-goog-api-key: " . $this->apiKey];

        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // Return the response as a string
        curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true); // Follow any redirects

        // Optional: Enable verbose cURL output for debugging network issues or detailed request/response.
        if ($this->debugCurl) {
            curl_setopt($ch, CURLOPT_VERBOSE, true);
            curl_setopt($ch, CURLOPT_STDERR, fopen('php://stderr', 'w')); // Direct verbose output to stderr
        }

        if ($method === 'POST') {
            curl_setopt($ch, CURLOPT_POST, true); // Set as a POST request
            curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload)); // Encode payload as JSON
            $headersToSend[] = "Content-Type: application/json"; // Specify content type for POST body
        }

        // Merge any additional custom headers provided by the caller.
        // This allows for flexible header customization without overriding essential API key/content type headers.
        $headersToSend = array_merge($headersToSend, $customHeaders);

        curl_setopt($ch, CURLOPT_HTTPHEADER, $headersToSend); // Set all compiled headers

        $response = curl_exec($ch); // Execute the cURL request
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE); // Get the HTTP status code
        $curlErrorNo = curl_errno($ch); // Get cURL error number
        $curlErrorMsg = curl_error($ch); // Get cURL error message

        curl_close($ch); // Close the cURL session

        // Handle cURL specific errors (e.g., network issues, timeouts, DNS resolution failures).
        if ($curlErrorNo !== 0) {
            throw new GeminiApiException(
                "cURL error ({$curlErrorNo}): {$curlErrorMsg}",
                0, // No HTTP status code for cURL-specific errors
                null,
                new Exception($curlErrorMsg, $curlErrorNo) // Wrap the original cURL error as a previous exception
            );
        }

        // Handle cases where cURL executes without error, but returns an empty response (e.g., connection reset).
        if ($response === false) {
             throw new GeminiApiException("Empty response received from API.", $httpCode);
        }

        $decodedResponse = json_decode($response, true); // Decode the JSON response into an associative array

        // Handle JSON decoding errors, which occur if the response is not valid JSON.
        if (json_last_error() !== JSON_ERROR_NONE) {
            throw new GeminiApiException(
                "Failed to decode JSON response: " . json_last_error_msg(),
                $httpCode,
                ['raw_response' => $response] // Include raw response for debugging JSON errors
            );
        }

        // Handle HTTP errors (status codes 4xx for client errors, 5xx for server errors).
        if ($httpCode >= 400 || $httpCode < 200) {
            // Extract a more specific error message from the API response if available.
            $errorMessage = $decodedResponse['error']['message'] ?? 'Unknown API error';
            throw new GeminiApiException(
                "API request failed with HTTP status {$httpCode}: {$errorMessage}",
                $httpCode,
                $decodedResponse // Include the full decoded response for detailed error context
            );
        }

        return $decodedResponse; // Return the successful decoded response
    }

    /**
     * Fetches metadata for a specific Gemini model.
     * This can be used to understand the model's capabilities, limits, and supported methods.
     *
     * @param string $modelName The name of the model (e.g., "gemini-2.5-flash"). Defaults to DEFAULT_MODEL_NAME.
     * @return array The decoded JSON response containing the model metadata.
     * @throws GeminiApiException If the API call to fetch model data fails.
     */
    public function getModelData(string $modelName = self::DEFAULT_MODEL_NAME): array
    {
        $endpoint = "/models/{$modelName}";
        // The API key is handled internally by _makeRequest via the x-goog-api-key header.
        return $this->_makeRequest($endpoint, 'GET');
    }

    /**
     * Generates content using the Gemini API based on a given prompt and configuration options.
     *
     * @param string $prompt The text prompt or user query for the content generation.
     * @param array $options Configuration options for generation, including:
     *    - 'instruction' (string|null): Optional system instruction to guide the model's behavior.
     *    - 'temperature' (float): Controls the randomness of the output. Higher values (e.g., 1.0-2.0) lead to more creative results. (0.0 - 2.0)
     *    - 'topP' (float): Nucleus sampling parameter. The model considers tokens whose cumulative probability exceeds this value. (0.0 - 1.0)
     *    - 'topK' (int): Top-k sampling parameter. The model considers the top 'k' most likely tokens. (positive integer)
     *    - 'thinkingBudget' (int): The budget in milliseconds for the model's internal "thinking" process. Use -1 for default.
     *    - 'includeThoughts' (bool): Whether to include the model's internal thought process in the response.
     *    - 'enableGrounding' (bool): Whether to enable Google Search grounding for the response, improving factual accuracy.
     * @return array The raw decoded API response, which typically includes 'candidates' with generated content.
     * @throws GeminiApiException If the API call to generate content fails.
     */
    public function generateContent(string $prompt, array $options = []): array
    {
        // Define default options to ensure all parameters have a sensible fallback.
        $defaultOptions = [
            'instruction' => null,
            'temperature' => 1.0,
            'topP' => 0.95,
            'topK' => 64,
            'thinkingBudget' => -1,
            'includeThoughts' => false,
            'enableGrounding' => false,
        ];

        // Merge user-provided options with defaults. User options will override defaults.
        $config = array_merge($defaultOptions, $options);

        // Construct the basic request payload structure.
        $payload = [
            "contents" => [
                [
                    "parts" => [
                        ["text" => $prompt]
                    ]
                ]
            ],
            "generationConfig" => [
                "temperature" => $config['temperature'],
                "topP" => $config['topP'],
                "topK" => $config['topK'],
            ],
        ];

        // Add system instruction to the payload if it's provided and not empty.
        if (!empty($config['instruction'])) {
            $payload['system_instruction'] = [
                "parts" => [
                    [
                        "text" => $config['instruction']
                    ]
                ]
            ];
        }

        // Add thinking configuration only if different from default, to avoid sending unnecessary fields.
        if ($config['thinkingBudget'] !== $defaultOptions['thinkingBudget'] || $config['includeThoughts'] !== $defaultOptions['includeThoughts']) {
             $payload['generationConfig']['thinkingConfig'] = [
                "thinkingBudget" => $config['thinkingBudget'],
                "includeThoughts" => $config['includeThoughts']
            ];
        }

        // Conditionally add the Google Search tool for grounding if enabled.
        if ($config['enableGrounding']) {
            $payload["tools"] = [
                [
                    "google_search" => (object)[] // Represents an empty Google Search tool configuration
                ]
            ];
        }

        $endpoint = "/models/" . self::DEFAULT_MODEL_NAME . ":generateContent";
        // The API key is handled internally by _makeRequest via the x-goog-api-key header.
        return $this->_makeRequest($endpoint, 'POST', $payload);
    }

    /**
     * Helper static method to extract and concatenate the generated text from a Gemini API response.
     * Useful for easily getting the main generated content from the structured response.
     *
     * @param array $responseData The full API response array obtained from generateContent().
     * @return string The concatenated generated text, or an empty string if no text parts are found.
     */
    public static function extractGeneratedText(array $responseData): string
    {
        // Check if the expected path to generated content parts exists.
        if (!isset($responseData['candidates'][0]['content']['parts'])) {
            return '';
        }

        $generatedText = [];
        // Iterate through all parts of the content, looking for 'text' components.
        foreach ($responseData['candidates'][0]['content']['parts'] as $part) {
            if (isset($part['text'])) {
                $generatedText[] = $part['text'];
            }
        }
        return implode("\n", $generatedText); // Join all text parts with newlines.
    }

    /**
     * Helper static method to extract grounding metadata (web search queries and source links)
     * from a Gemini API response.
     *
     * @param array $responseData The full API response array obtained from generateContent().
     * @return array An associative array containing:
     *   - 'webSearchQueries' (array of strings): List of search queries used for grounding.
     *   - 'sourceLinks' (array of strings): List of URIs of the sources used for grounding.
     *   Returns empty arrays if no grounding metadata is present.
     */
    public static function extractGroundingMetadata(array $responseData): array
    {
        $metadata = [
            'webSearchQueries' => [],
            'sourceLinks' => [],
        ];

        // Check if grounding metadata exists in the response.
        if (isset($responseData['candidates'][0]['groundingMetadata'])) {
            $groundingMetadata = $responseData['candidates'][0]['groundingMetadata'];

            // Extract web search queries if available.
            if (isset($groundingMetadata['webSearchQueries'])) {
                $metadata['webSearchQueries'] = $groundingMetadata['webSearchQueries'];
            }

            // Extract source URIs from grounding chunks if available.
            if (isset($groundingMetadata['groundingChunks'])) {
                foreach ($groundingMetadata['groundingChunks'] as $chunk) {
                    if (isset($chunk['web']) && isset($chunk['web']['uri'])) {
                        $metadata['sourceLinks'][] = $chunk['web']['uri'];
                    }
                }
            }
        }
        return $metadata;
    }
}

// --- Main script execution ---

// It is highly recommended to set your API key as an environment variable
// rather than hardcoding it for security reasons.
// Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
// Example for Windows (Command Prompt): set GEMINI_API_KEY=YOUR_API_KEY
// Example for Windows (PowerShell): $env:GEMINI_API_KEY="YOUR_API_KEY"
$apiKey = getenv("GEMINI_API_KEY");

// Check if the API key environment variable is set.
// If not, print an error message and exit to prevent API calls from failing.
if (!$apiKey) {
    echo "Error: GEMINI_API_KEY environment variable not set.\n";
    echo "Please set it using one of the following methods before running the script:\n";
    echo "  Linux/macOS: export GEMINI_API_KEY=\"YOUR_API_KEY\"\n";
    echo "  Windows (Command Prompt): set GEMINI_API_KEY=YOUR_API_KEY\n";
    echo "  Windows (PowerShell): \$env:GEMINI_API_KEY=\"YOUR_API_KEY\"\n";
    exit(1); // Exit with a non-zero status code indicating an error
}

try {
    // Instantiate the Gemini API client.
    // The second argument ($debugCurl) can be set to 'true' to enable verbose cURL debugging output,
    // which is helpful for diagnosing network or request issues.
    $geminiClient = new GeminiApiClient($apiKey, $debugCurl = false);

    // --- Getting the Model Data section ---
    echo "\n--- Fetching Model Data ---\n";
    // Retrieve metadata for the default Gemini model.
    $modelData = $geminiClient->getModelData();

    // Print the model metadata in a readable format.
    foreach ($modelData as $key => $value) {
        echo $key . ": " . (is_array($value) ? json_encode($value) : $value) . "\n";
    }

    // --- Requesting a response section ---
    echo "\n--- Requesting AI Content ---\n";

    $promptText = "Explain the concept of LLM embeddings in simple terms.";
    $instruction = "You are a helpful AI assistant with a flourishing mindset";

    // Configure generation options for the content request.
    // Adjust these values to control the behavior of the Gemini model.
    $generationOptions = [
        'instruction' => $instruction,    // System instruction for the model
        'temperature' => 1.0,             // Controls randomness (0.0 to 2.0)
        'topP' => 0.95,                   // Nucleus sampling parameter
        'topK' => 64,                     // Top-k sampling parameter
        'thinkingBudget' => -1,           // Thinking budget in milliseconds (-1 for default)
        'includeThoughts' => false,       // Set to true to include the model's internal thoughts
        'enableGrounding' => false,       // Set to 'true' to enable Google Search grounding
    ];

    // Make the content generation request to the Gemini API.
    $response = $geminiClient->generateContent($promptText, $generationOptions);

    echo "\n--- Response Data ---\n";

    // Extract and print the generated text using the static helper method.
    echo "Generated Text:\n\n";
    echo GeminiApiClient::extractGeneratedText($response) . "\n";
    echo str_repeat("-", 50) . "\n"; // Optional separator for readability

    // Check if grounding metadata is present (only if enableGrounding was true and sources were found).
    // If so, extract and print the search queries and source links.
    $groundingInfo = GeminiApiClient::extractGroundingMetadata($response);
    if (!empty($groundingInfo['webSearchQueries']) || !empty($groundingInfo['sourceLinks'])) {
        echo "\n--- Google Grounding Info ---\n";

        if (!empty($groundingInfo['webSearchQueries'])) {
            echo "Search Queries:\n";
            foreach ($groundingInfo['webSearchQueries'] as $query) {
                echo "  - " . $query . "\n";
            }
        }

        if (!empty($groundingInfo['sourceLinks'])) {
            echo "\nSource Links:\n";
            foreach ($groundingInfo['sourceLinks'] as $uri) {
                echo "  - " . $uri . "\n";
            }
        }
        echo str_repeat("-", 50) . "\n"; // Optional separator
    }

    // --- Summary of Request Parameters ---
    // Print a summary of the values used for the current content generation request.
    echo "\nSummary of values set for this request\n";
    echo "---------------------------------------\n";
    echo "Prompt: " . trim($promptText) . "\n";
    echo "Instruction: " . trim($generationOptions['instruction'] ?? 'N/A') . "\n";
    echo "Temperature: " . $generationOptions['temperature'] . "\n";
    echo "Top P: " . $generationOptions['topP'] . "\n";
    echo "Top K: " . $generationOptions['topK'] . "\n";
    echo "Thinking Budget: " . $generationOptions['thinkingBudget'] . "\n";
    echo "Include Thoughts: " . ($generationOptions['includeThoughts'] ? 'true' : 'false') . "\n";
    echo "Enable Grounding: " . ($generationOptions['enableGrounding'] ? 'true' : 'false') . "\n";
    echo "---------------------------------------\n";

} catch (InvalidArgumentException $e) {
    // Catch specific errors related to invalid arguments, like an empty API key during client instantiation.
    echo "Configuration Error: " . $e->getMessage() . "\n";
    exit(1);
} catch (GeminiApiException $e) {
    // Catch errors specific to the Gemini API interactions (network issues, HTTP errors, JSON parsing errors).
    echo "API Error: " . $e->getMessage() . "\n";
    echo "HTTP Status: " . $e->getHttpStatusCode() . "\n";
    if ($e->getApiResponse()) {
        echo "API Response Details:\n" . json_encode($e->getApiResponse(), JSON_PRETTY_PRINT) . "\n";
    }
    exit(1);
} catch (Exception $e) {
    // Catch any other unexpected general errors that might occur during script execution.
    echo "An unexpected error occurred: " . $e->getMessage() . "\n";
    exit(1);
}

?>