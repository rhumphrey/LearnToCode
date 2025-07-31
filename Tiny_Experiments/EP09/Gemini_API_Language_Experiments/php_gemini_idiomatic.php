<?php

/**
 * Custom exception class for Gemini API errors.
 */
class GeminiApiException extends Exception
{
    protected ?array $apiResponse;
    protected int $httpStatusCode;

    public function __construct(
        string $message,
        int $httpStatusCode = 0,
        ?array $apiResponse = null,
        ?Throwable $previous = null // Explicitly marked as nullable for PHP 8+
    ) {
        parent::__construct($message, $httpStatusCode, $previous);
        $this->httpStatusCode = $httpStatusCode;
        $this->apiResponse = $apiResponse;
    }

    public function getApiResponse(): ?array
    {
        return $this->apiResponse;
    }

    public function getHttpStatusCode(): int
    {
        return $this->httpStatusCode;
    }
}

/**
 * A client for interacting with the Gemini API.
 */
class GeminiApiClient
{
    private const BASE_URL = "https://generativelanguage.googleapis.com/v1beta";
    private const DEFAULT_MODEL_NAME = "gemini-2.5-flash"; // Default model

    private string $apiKey;
    private bool $debugCurl;

    /**
     * GeminiApiClient constructor.
     * @param string $apiKey Your Gemini API key.
     * @param bool $debugCurl Set to true to enable verbose cURL output for debugging.
     * @throws InvalidArgumentException If the API key is empty.
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
     * Makes an HTTP request to the Gemini API.
     *
     * @param string $endpoint The API endpoint (e.g., "/models/gemini-2.5-flash:generateContent").
     * @param string $method The HTTP method (GET or POST).
     * @param array $payload The request body for POST requests.
     * @param array $customHeaders Additional HTTP headers.
     * @return array The decoded JSON response.
     * @throws GeminiApiException If the request fails or returns a non-200 status code.
     */
    private function _makeRequest(string $endpoint, string $method, array $payload = [], array $customHeaders = []): array
    {
        $url = self::BASE_URL . $endpoint;
        $ch = curl_init();

        // Start with the required API key header
        $headersToSend = ["x-goog-api-key: " . $this->apiKey];

        // For GET requests, the API key is typically handled via the x-goog-api-key header.
        // The original code used a ?key= query parameter for model data, but the header is more idiomatic
        // for Google APIs and works universally.
        // No specific URL query parameters are added here for the API key.

        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true); // Follow redirects

        // Optional: Enable verbose cURL output for debugging
        if ($this->debugCurl) {
            curl_setopt($ch, CURLOPT_VERBOSE, true);
            curl_setopt($ch, CURLOPT_STDERR, fopen('php://stderr', 'w'));
        }

        if ($method === 'POST') {
            curl_setopt($ch, CURLOPT_POST, true);
            curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
            $headersToSend[] = "Content-Type: application/json"; // Add content type for POST requests
        }

        // Merge any additional custom headers provided by the caller
        // Note: This merging assumes customHeaders do not override essential headers like x-goog-api-key
        // or Content-Type (which is generally good practice).
        $headersToSend = array_merge($headersToSend, $customHeaders);

        curl_setopt($ch, CURLOPT_HTTPHEADER, $headersToSend);

        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        $curlErrorNo = curl_errno($ch);
        $curlErrorMsg = curl_error($ch);

        curl_close($ch);

        // Handle cURL specific errors (e.g., network issues, timeouts)
        if ($curlErrorNo !== 0) {
            throw new GeminiApiException(
                "cURL error ({$curlErrorNo}): {$curlErrorMsg}",
                0, // No HTTP status code for cURL errors
                null,
                new Exception($curlErrorMsg, $curlErrorNo) // Original cURL error as previous exception
            );
        }

        // Handle empty response
        if ($response === false) {
             throw new GeminiApiException("Empty response received from API.", $httpCode);
        }

        $decodedResponse = json_decode($response, true);

        // Handle JSON decoding errors
        if (json_last_error() !== JSON_ERROR_NONE) {
            throw new GeminiApiException(
                "Failed to decode JSON response: " . json_last_error_msg(),
                $httpCode,
                ['raw_response' => $response]
            );
        }

        // Handle HTTP errors (4xx, 5xx status codes)
        if ($httpCode >= 400 || $httpCode < 200) {
            $errorMessage = $decodedResponse['error']['message'] ?? 'Unknown API error';
            throw new GeminiApiException(
                "API request failed with HTTP status {$httpCode}: {$errorMessage}",
                $httpCode,
                $decodedResponse
            );
        }

        return $decodedResponse;
    }

    /**
     * Fetches metadata for a specific Gemini model.
     *
     * @param string $modelName The name of the model (e.g., "gemini-2.5-flash").
     * @return array The model metadata.
     * @throws GeminiApiException
     */
    public function getModelData(string $modelName = self::DEFAULT_MODEL_NAME): array
    {
        $endpoint = "/models/{$modelName}";
        return $this->_makeRequest($endpoint, 'GET'); // API key handled by _makeRequest
    }

    /**
     * Generates content using the Gemini API.
     *
     * @param string $prompt The text prompt for generation.
     * @param array $options Configuration options for generation:
     *    - instruction (string|null): System instruction for the model.
     *    - temperature (float): Controls randomness (0.0 - 2.0).
     *    - topP (float): Nucleus sampling parameter (0.0 - 1.0).
     *    - topK (int): Top-k sampling parameter (positive integer).
     *    - thinkingBudget (int): Thinking budget for the model (milliseconds, -1 for default).
     *    - includeThoughts (bool): Whether to include model's internal thoughts in the response.
     *    - enableGrounding (bool): Whether to enable Google Search grounding.
     * @return array The API response, including generated content and metadata.
     * @throws GeminiApiException
     */
    public function generateContent(string $prompt, array $options = []): array
    {
        $defaultOptions = [
            'instruction' => null,
            'temperature' => 1.0,
            'topP' => 0.95,
            'topK' => 64,
            'thinkingBudget' => -1,
            'includeThoughts' => false,
            'enableGrounding' => false,
        ];

        $config = array_merge($defaultOptions, $options);

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

        // Add system instruction if provided
        if (!empty($config['instruction'])) {
            $payload['system_instruction'] = [
                "parts" => [
                    [
                        "text" => $config['instruction']
                    ]
                ]
            ];
        }

        // Add thinking config if thinkingBudget or includeThoughts is explicitly set
        if ($config['thinkingBudget'] !== $defaultOptions['thinkingBudget'] || $config['includeThoughts'] !== $defaultOptions['includeThoughts']) {
             $payload['generationConfig']['thinkingConfig'] = [
                "thinkingBudget" => $config['thinkingBudget'],
                "includeThoughts" => $config['includeThoughts']
            ];
        }

        // Handle grounding (Google Search tool)
        if ($config['enableGrounding']) {
            $payload["tools"] = [
                [
                    "google_search" => (object)[] // Empty object for Google Search tool
                ]
            ];
        }

        $endpoint = "/models/" . self::DEFAULT_MODEL_NAME . ":generateContent";
        return $this->_makeRequest($endpoint, 'POST', $payload); // API key handled by _makeRequest
    }

    /**
     * Helper to extract the concatenated generated text from the API response.
     *
     * @param array $responseData The full API response array.
     * @return string The concatenated generated text, or an empty string if not found.
     */
    public static function extractGeneratedText(array $responseData): string
    {
        if (!isset($responseData['candidates'][0]['content']['parts'])) {
            return '';
        }

        $generatedText = [];
        foreach ($responseData['candidates'][0]['content']['parts'] as $part) {
            if (isset($part['text'])) {
                $generatedText[] = $part['text'];
            }
        }
        return implode("\n", $generatedText);
    }

    /**
     * Helper to extract grounding metadata (web search queries and source links).
     *
     * @param array $responseData The full API response array.
     * @return array An associative array with 'webSearchQueries' (array of strings) and 'sourceLinks' (array of URIs).
     */
    public static function extractGroundingMetadata(array $responseData): array
    {
        $metadata = [
            'webSearchQueries' => [],
            'sourceLinks' => [],
        ];

        if (isset($responseData['candidates'][0]['groundingMetadata'])) {
            $groundingMetadata = $responseData['candidates'][0]['groundingMetadata'];

            if (isset($groundingMetadata['webSearchQueries'])) {
                $metadata['webSearchQueries'] = $groundingMetadata['webSearchQueries'];
            }

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
$apiKey = getenv("GEMINI_API_KEY");

// Check if the API key environment variable is set
if (!$apiKey) {
    echo "Error: GEMINI_API_KEY environment variable not set.\n";
    echo "Please set it using: \n";
    echo "  Linux/macOS: export GEMINI_API_KEY=\"YOUR_API_KEY\"\n";
    echo "  Windows (Command Prompt): set GEMINI_API_KEY=YOUR_API_KEY\n";
    echo "  Windows (PowerShell): \$env:GEMINI_API_KEY=\"YOUR_API_KEY\"\n";
    exit(1); // Exit with a non-zero status code indicating an error
}

try {
    // Instantiate the client. Set the second argument to 'true' for verbose cURL debugging.
    $geminiClient = new GeminiApiClient($apiKey, $debugCurl = false);

    // --- Getting the Model Data section ---
    echo "\n--- Fetching Model Data ---\n";
    $modelData = $geminiClient->getModelData();

    // Print the model metadata
    foreach ($modelData as $key => $value) {
        echo $key . ": " . (is_array($value) ? json_encode($value) : $value) . "\n";
    }

    // --- Requesting a response section ---
    echo "\n--- Requesting AI Content ---\n";

    $promptText = "Explain the concept of LLM embeddings in simple terms.";
    $instruction = "You are a helpful AI assistant with a flourishing mindset";

    // Configure generation options
    $generationOptions = [
        'instruction' => $instruction,
        'temperature' => 1.0,
        'topP' => 0.95,
        'topK' => 64,
        'thinkingBudget' => -1,       // Use default
        'includeThoughts' => false,   // Do not include thoughts
        'enableGrounding' => false,   // Set to 'true' to enable Google Search grounding
    ];

    $response = $geminiClient->generateContent($promptText, $generationOptions);

    echo "\n--- Response Data ---\n";

    // Extract and print generated text using the helper
    echo "Generated Text:\n\n";
    echo GeminiApiClient::extractGeneratedText($response) . "\n";
    echo str_repeat("-", 50) . "\n";

    // Check if grounding metadata is present and print it using the helper
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
        echo str_repeat("-", 50) . "\n";
    }

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
    // Catch issues with client instantiation (e.g., empty API key provided)
    echo "Configuration Error: " . $e->getMessage() . "\n";
    exit(1);
} catch (GeminiApiException $e) {
    // Catch API-specific errors (network, HTTP status, JSON decoding)
    echo "API Error: " . $e->getMessage() . "\n";
    echo "HTTP Status: " . $e->getHttpStatusCode() . "\n";
    if ($e->getApiResponse()) {
        echo "API Response Details:\n" . json_encode($e->getApiResponse(), JSON_PRETTY_PRINT) . "\n";
    }
    exit(1);
} catch (Exception $e) {
    // Catch any other unexpected general errors
    echo "An unexpected error occurred: " . $e->getMessage() . "\n";
    exit(1);
}

?>