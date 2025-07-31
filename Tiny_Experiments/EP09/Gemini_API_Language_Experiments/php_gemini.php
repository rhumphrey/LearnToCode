<?php

// It is highly recommended to set your API key as an environment variable
// Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
// Example for Windows (Command Prompt): set GEMINI_API_KEY=YOUR_API_KEY
// Example for Windows (PowerShell): $env:GEMINI_API_KEY="YOUR_API_KEY"
$apiKey = getenv("GEMINI_API_KEY");

if (!$apiKey) {
    die("Error: GEMINI_API_KEY environment variable not set.\n");
}

$geminiApiEndpoint = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent";
$geminiModelEndpoint = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash";

// --- Getting the Model Data section ---

$params = [
    "key" => $apiKey
];

$ch = curl_init();

// In your PHP script, after curl_init($ch)
curl_setopt($ch, CURLOPT_VERBOSE, true); // Enables verbose output, showing connection details
curl_setopt($ch, CURLOPT_STDERR, fopen('php://stderr', 'w')); // Directs verbose output to stderr
// You might also add a timeout if you suspect that's the issue
// curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 10); // 10 seconds to connect
// curl_setopt($ch, CURLOPT_TIMEOUT, 30); // 30 seconds for the entire transfer


curl_setopt($ch, CURLOPT_URL, $geminiModelEndpoint . '?' . http_build_query($params));
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);
$httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
curl_close($ch);

// After curl_exec($ch);
if ($httpCode === 0) { // Check for HTTP 0 specifically
    echo "cURL error number: " . curl_errno($ch) . "\n";
    echo "cURL error message: " . curl_error($ch) . "\n";
}

if ($httpCode !== 200) {
    die("Error fetching model data: HTTP " . $httpCode . "\n" . $response . "\n");
}

$modelData = json_decode($response, true);

// Print the model metadata
echo "\n--- Default Model Data Before Any Changes ---\n";
foreach ($modelData as $key => $value) {
    echo $key . ": " . (is_array($value) ? json_encode($value) : $value) . "\n";
}

echo "\nPress any key to continue\n";
// This is a simple way to pause execution, similar to Python's input()
// In a web context, you wouldn't typically do this.
fgets(STDIN);

// --- Requesting a response section ---

$text = "Explain the concept of LLM embeddings in simple terms.";
$instruction = "You are a helpful AI assistant with a flourishing mindset";
$temperature = 1;
$topP = 0.95;
$topK = 64;
$thinkingBudget = -1;
$includeThoughts = false;
$enableGrounding = false;

$headers = [
    "x-goog-api-key: " . $apiKey,
    "Content-Type: application/json"
];

$requestPayload = [
    "system_instruction" => [
        "parts" => [
            [
                "text" => $instruction
            ]
        ]
    ],
    "contents" => [
        [
            "parts" => [
                ["text" => $text]
            ]
        ]
    ],
    "generationConfig" => [
        "temperature" => $temperature,
        "topP" => $topP,
        "topK" => $topK,
        "thinkingConfig" => [
            "thinkingBudget" => $thinkingBudget,
            "includeThoughts" => $includeThoughts
        ]
    ],
    "tools" => [
        [
            "google_search" => (object)[]
        ]
    ]
];

if ($enableGrounding) {
    if (!isset($requestPayload["tools"])) {
        $requestPayload["tools"] = [
            [
                "google_search" => (object)[]
            ]
        ];
    }
} else {
    if (isset($requestPayload["tools"])) {
        unset($requestPayload["tools"]);
    }
}

// Send the POST request
echo "Sending Request...\n";
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $geminiApiEndpoint);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_POST, true);
curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($requestPayload));
curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
$response = curl_exec($ch);
$httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
curl_close($ch);

if ($httpCode !== 200) {
    die("Error requesting content: HTTP " . $httpCode . "\n" . $response . "\n");
}

// Parse the JSON response body into a PHP array
$responseData = json_decode($response, true);

// Extract the generated text from the structured response
$generatedParts = $responseData["candidates"][0]["content"]["parts"];

echo "\n--- Response Data ---\n";

// Deal with potentially multiple parts of text (affected by 'include thoughts' setting)
echo "Generated Text:\n\n";
foreach ($generatedParts as $part) {
    if (isset($part["text"])) {
        echo $part["text"] . "\n";
        echo str_repeat("-", 50) . "\n"; // Optional: Add a separator for clarity
    }
}

// Check if grounding metadata is present in the response
if (isset($responseData["candidates"][0]["groundingMetadata"])) {
    $groundingMetadata = $responseData["candidates"][0]["groundingMetadata"];

    echo "\n--- Google Grounding Info ---\n";

    // Print search queries
    if (isset($groundingMetadata["webSearchQueries"])) {
        echo "Search Queries:\n";
        foreach ($groundingMetadata["webSearchQueries"] as $query) {
            echo "  - " . $query . "\n";
        }
    }

    // Print source links
    if (isset($groundingMetadata["groundingChunks"])) {
        echo "\nSource Links:\n";
        foreach ($groundingMetadata["groundingChunks"] as $chunk) {
            if (isset($chunk["web"]) && isset($chunk["web"]["uri"])) {
                echo "  - " . $chunk["web"]["uri"] . "\n";
            }
        }
    }
    echo str_repeat("-", 50) . "\n";
}


echo "\nSummary of values set for this request\n";
echo "---------------------------------------\n";
echo "text: " . trim($text) . "\n";
echo "instruction: " . trim($instruction) . "\n";
echo "temperature: " . $temperature . "\n";
echo "topP: " . $topP . "\n";
echo "topK: " . $topK . "\n";
echo "thinking_budget: " . $thinkingBudget . "\n";
echo "include_thoughts: " . ($includeThoughts ? 'true' : 'false') . "\n";
echo "enable_grounding: " . ($enableGrounding ? 'true' : 'false') . "\n";
echo "---------------------------------------\n";

?>