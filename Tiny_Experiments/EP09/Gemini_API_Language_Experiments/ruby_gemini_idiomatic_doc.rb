require 'net/http'
require 'uri'
require 'json'

# --- Environment Variable Check ---
# It is highly recommended to set your API key as an environment variable for security.
# This check ensures that the GEMINI_API_KEY is available before proceeding.
# Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
# Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
unless ENV['GEMINI_API_KEY']
  puts "Error: GEMINI_API_KEY environment variable not set."
  exit 1
end

# This class encapsulates the logic for interacting with the Google Gemini API.
# It handles the construction of HTTP requests and parsing of responses.
class GeminiClient
  # The base URI for the Gemini API model endpoint.
  BASE_URI = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash".freeze
  
  # The API key is retrieved from environment variables.
  API_KEY = ENV['GEMINI_API_KEY']

  # Initializes the GeminiClient.
  def initialize
    @api_key = API_KEY
  end

  # Fetches metadata for the specified Gemini model.
  # @return [Hash] A hash containing the model's metadata.
  def get_model_data
    uri = URI(BASE_URI)
    uri.query = URI.encode_www_form({ key: @api_key })
    response = Net::HTTP.get_response(uri)
    JSON.parse(response.body)
  end

  # Sends a request to generate content based on the provided payload.
  # @param payload [Hash] The request payload for the generateContent endpoint.
  # @return [Hash] The parsed JSON response from the API.
  def generate_content(payload)
    uri = URI("#{BASE_URI}:generateContent")
    headers = {
      "x-goog-api-key" => @api_key,
      "Content-Type" => "application/json"
    }

    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true # Ensures a secure connection
    request = Net::HTTP::Post.new(uri.request_uri, headers)
    request.body = payload.to_json

    response = http.request(request)
    JSON.parse(response.body)
  end
end

# --- Helper Methods for Output ---

# Prints the model metadata in a readable format.
# @param model_data [Hash] The model data to be printed.
def print_model_data(model_data)
  puts "\n--- Default Model Data Before Any Changes ---"
  model_data.each do |key, value|
    puts "#{key}: #{value}"
  end
end

# Parses and prints the generated content from the API response.
# @param response_data [Hash] The full response from the generate_content call.
def print_response_data(response_data)
  puts "\n--- Response Data ---"
  # Safely access nested parts of the response using .dig
  generated_parts = response_data.dig("candidates", 0, "content", "parts")

  if generated_parts
    puts "Generated Text:\n"
    generated_parts.each do |part|
      puts part["text"] if part.key?("text")
      puts "-" * 50
    end
  end

  grounding_metadata = response_data.dig("candidates", 0, "groundingMetadata")
  print_grounding_info(grounding_metadata) if grounding_metadata
end

# Prints the grounding information (search queries and source links) if available.
# @param grounding_metadata [Hash] The grounding metadata from the API response.
def print_grounding_info(grounding_metadata)
  puts "\n--- Google Grounding Info ---"

  if grounding_metadata["webSearchQueries"]
    puts "Search Queries:"
    grounding_metadata["webSearchQueries"].each { |query| puts "  - #{query}" }
  end

  if grounding_metadata["groundingChunks"]
    puts "\nSource Links:"
    grounding_metadata["groundingChunks"].each do |chunk|
      puts "  - #{chunk.dig('web', 'uri')}" if chunk.dig('web', 'uri')
    end
  end
  puts "-" * 50
end

# Prints a summary of the parameters used for the content generation request.
# @param params [Hash] The parameters used to generate the content.
def print_request_summary(params)
  puts "\nSummary of values set for this request"
  puts "---------------------------------------"
  params.each do |key, value|
    # Strips whitespace from string values for cleaner output
    puts "#{key}: #{value.is_a?(String) ? value.strip : value}"
  end
  puts "---------------------------------------"
end

# --- Main Execution ---

# Create a new instance of the GeminiClient.
client = GeminiClient.new

# 1. Get and print the model's metadata.
model_data = client.get_model_data
print_model_data(model_data)

puts "\nPress any key to continue\n"
gets

# 2. Configure the parameters for the content generation request.
generation_params = {
  text: "Explain the concept of LLM embeddings in simple terms.",
  instruction: "You are a helpful AI assistant with a flourishing mindset",
  temperature: 1.0,
  top_p: 0.95,
  top_k: 64,
  thinking_budget: -1,      # A value of -1 indicates no specific budget for thinking time.
  include_thoughts: false,   # Set to true to receive reasoning and planning steps from the model.
  enable_grounding: false    # Set to true to enable grounding with Google Search.
}

# 3. Build the request payload from the generation parameters.
request_payload = {
  "system_instruction" => { "parts" => [{ "text" => generation_params[:instruction] }] },
  "contents" => [{ "parts" => [{ "text" => generation_params[:text] }] }],
  "generationConfig" => {
    "temperature" => generation_params[:temperature],
    "topP" => generation_params[:top_p],
    "topK" => generation_params[:top_k],
    "thinkingConfig" => {
      "thinkingBudget" => generation_params[:thinking_budget],
      "includeThoughts" => generation_params[:include_thoughts]
    }
  }
}

# Conditionally add the tools for grounding if it's enabled.
request_payload["tools"] = [{ "google_search" => {} }] if generation_params[:enable_grounding]

# 4. Send the request to the API and get the response.
puts "Sending Request..."
response_data = client.generate_content(request_payload)

# 5. Print the formatted response data.
print_response_data(response_data)

# 6. Print a summary of the request parameters for reference.
print_request_summary(generation_params)