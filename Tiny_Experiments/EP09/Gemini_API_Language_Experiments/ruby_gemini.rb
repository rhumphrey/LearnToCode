require 'net/http'
require 'uri'
require 'json'

# It is highly recommended to set your API key as an environment variable
# Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
# Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
API_KEY = ENV['GEMINI_API_KEY']

GEMINI_API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
GEMINI_MODEL_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash"

# Getting the Model Data section
uri = URI(GEMINI_MODEL_ENDPOINT)
uri.query = URI.encode_www_form({ key: API_KEY })

response = Net::HTTP.get_response(uri)
model_data = JSON.parse(response.body)

# Print the model metadata
puts "\n--- Default Model Data Before Any Changes ---"
model_data.each do |key, value|
  puts "#{key}: #{value}"
end

puts "\nPress any key to continue\n"
gets

# Requesting a response section
text = """
Explain the concept of LLM embeddings in simple terms.
"""

instruction = """
You are a helpful AI assistant with a flourishing mindset
"""

temperature = 1
top_p = 0.95
top_k = 64

thinking_budget = -1
include_thoughts = false

enable_grounding = false

headers = {
  "x-goog-api-key" => API_KEY,
  "Content-Type" => "application/json"
}

request_payload = {
  "system_instruction" => {
    "parts" => [
      {
        "text" => instruction
      }
    ]
  },
  "contents" => [
    {
      "parts" => [
        { "text" => text }
      ]
    }
  ],
  "generationConfig" => {
    "temperature" => temperature,
    "topP" => top_p,
    "topK" => top_k,
    "thinkingConfig" => {
      "thinkingBudget" => thinking_budget,
      "includeThoughts" => include_thoughts
    }
  }
}

if enable_grounding
  request_payload["tools"] = [{ "google_search" => {} }]
else
  request_payload.delete("tools")
end

# Send the POST request
puts "Sending Request..."
uri = URI(GEMINI_API_ENDPOINT)
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true
request = Net::HTTP::Post.new(uri.request_uri, headers)
request.body = request_payload.to_json

response = http.request(request)

# Parse the JSON response body into a Ruby hash
response_data = JSON.parse(response.body)

# Extract the generated text from the structured response
generated_parts = response_data["candidates"][0]["content"]["parts"]

puts "\n--- Response Data ---"

# Deal with potentially multiple parts of text (affected by 'include thoughts' setting)
puts "Generated Text:\n"
generated_parts.each do |part|
  if part.key?("text")
    puts part["text"]
    puts "-" * 50 # Optional: Add a separator for clarity between different text parts
  end
end

# Check if grounding metadata is present in the response
if response_data["candidates"][0].key?("groundingMetadata")
  grounding_metadata = response_data["candidates"][0]["groundingMetadata"]

  puts "\n--- Google Grounding Info ---"

  # Print search queries
  if grounding_metadata.key?("webSearchQueries")
    puts "Search Queries:"
    grounding_metadata["webSearchQueries"].each do |query|
      puts "  - #{query}"
    end
  end

  # Print source links
  if grounding_metadata.key?("groundingChunks")
    puts "\nSource Links:"
    grounding_metadata["groundingChunks"].each do |chunk|
      if chunk.key?("web") && chunk["web"].key?("uri")
        puts "  - #{chunk['web']['uri']}"
      end
    end
  end

  puts "-" * 50
end

puts "\nSummary of values set for this request"
puts "---------------------------------------"
puts "text: #{text.strip}"
puts "instruction: #{instruction.strip}"
puts "temperature: #{temperature}"
puts "topP: #{top_p}"
puts "topK: #{top_k}"
puts "thinking_budget: #{thinking_budget}"
puts "include_thoughts: #{include_thoughts}"
puts "enable_grounding: #{enable_grounding}"
puts "---------------------------------------"