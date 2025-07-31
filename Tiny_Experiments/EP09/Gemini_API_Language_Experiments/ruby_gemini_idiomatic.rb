require 'net/http'
require 'uri'
require 'json'

# It is highly recommended to set your API key as an environment variable
# Example for Linux/macOS: export GEMINI_API_KEY="YOUR_API_KEY"
# Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY
unless ENV['GEMINI_API_KEY']
  puts "Error: GEMINI_API_KEY environment variable not set."
  exit 1
end

class GeminiClient
  BASE_URI = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash".freeze
  API_KEY = ENV['GEMINI_API_KEY']

  def initialize
    @api_key = API_KEY
  end

  def get_model_data
    uri = URI(BASE_URI)
    uri.query = URI.encode_www_form({ key: @api_key })
    response = Net::HTTP.get_response(uri)
    JSON.parse(response.body)
  end

  def generate_content(payload)
    uri = URI("#{BASE_URI}:generateContent")
    headers = {
      "x-goog-api-key" => @api_key,
      "Content-Type" => "application/json"
    }

    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true
    request = Net::HTTP::Post.new(uri.request_uri, headers)
    request.body = payload.to_json

    response = http.request(request)
    JSON.parse(response.body)
  end
end

def print_model_data(model_data)
  puts "\n--- Default Model Data Before Any Changes ---"
  model_data.each do |key, value|
    puts "#{key}: #{value}"
  end
end

def print_response_data(response_data)
  puts "\n--- Response Data ---"
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

def print_request_summary(params)
  puts "\nSummary of values set for this request"
  puts "---------------------------------------"
  params.each do |key, value|
    puts "#{key}: #{value.is_a?(String) ? value.strip : value}"
  end
  puts "---------------------------------------"
end

# --- Main Execution ---
client = GeminiClient.new

# Get and print model data
model_data = client.get_model_data
print_model_data(model_data)

puts "\nPress any key to continue\n"
gets

# Configuration for the content generation
generation_params = {
  text: "Explain the concept of LLM embeddings in simple terms.",
  instruction: "You are a helpful AI assistant with a flourishing mindset",
  temperature: 1.0,
  top_p: 0.95,
  top_k: 64,
  thinking_budget: -1,
  include_thoughts: false,
  enable_grounding: false
}

# Build the request payload
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

request_payload["tools"] = [{ "google_search" => {} }] if generation_params[:enable_grounding]

# Send the request and print the response
puts "Sending Request..."
response_data = client.generate_content(request_payload)
print_response_data(response_data)

# Print the summary of the request
print_request_summary(generation_params)