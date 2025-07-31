use strict; # Enforces stricter parsing rules, catching common errors.
use warnings; # Enables Perl's built-in warnings, crucial for good practice.

# LWP::UserAgent is a powerful and flexible HTTP user agent.
# It's used to make web requests (GET, POST, etc.) to the Gemini API.
use LWP::UserAgent;

# JSON module for encoding Perl data structures into JSON strings
# and decoding JSON strings back into Perl data structures.
# We explicitly import 'encode_json' and 'decode_json' for clarity.
use JSON qw(encode_json decode_json);

# URI module for working with URIs (Uniform Resource Identifiers).
# Used here to construct the URL for fetching model data with query parameters.
use URI;

# Carp module provides functions like 'croak' which are similar to 'die'
# but report errors from the perspective of the caller, making debugging easier.
use Carp qw(croak);

# 'say' feature automatically adds a newline character at the end of the print statement,
# making output cleaner and more concise than traditional 'print "$var\n"'.
use feature 'say';

# --- Constants ---
# Using 'use constant' defines compile-time constants.
# This makes values immutable, prevents accidental modification,
# and improves code readability by giving meaningful names to fixed values.

# The API endpoint for generating content with the Gemini 2.5 Flash model.
use constant GEMINI_API_ENDPOINT   => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent";
# The API endpoint for fetching metadata about the Gemini 2.5 Flash model.
use constant GEMINI_MODEL_ENDPOINT => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash";
# Default timeout for HTTP requests in seconds.
use constant DEFAULT_TIMEOUT       => 120; # 120 seconds = 2 minutes

# Global User Agent object.
# Initialized once to be reused across all API calls,
# avoiding redundant object creation and maintaining connection settings.
my $ua;

# --- Subroutines for better organization and reusability ---
# Each subroutine performs a specific task, improving modularity and testability.

=head2 get_api_key()

Fetches the Gemini API key from the 'GEMINI_API_KEY' environment variable.

This function ensures that the API key is defined and not empty, providing
clear instructions if it's missing. Using environment variables is a
recommended security practice to avoid hardcoding sensitive information
directly in the script.

Args: None.

Returns: The API key (string).

Croaks: If the 'GEMINI_API_KEY' environment variable is not set or is empty.

=cut
sub get_api_key {
    my $api_key = $ENV{"GEMINI_API_KEY"};
    unless (defined $api_key && length $api_key > 0) {
        # Using croak from Carp module to report the error from the caller's perspective.
        croak "Error: GEMINI_API_KEY environment variable is not set or is empty.\n" .
              "Please set it before running.\n" .
              "Example for Linux/macOS: export GEMINI_API_KEY=\"YOUR_API_KEY\"\n" .
              "Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY\n";
    }
    return $api_key;
}

=head2 init_user_agent()

Initializes the global L<LWP::UserAgent> object with a default timeout.

This subroutine sets up the HTTP client that will be used for all
web requests in the script.

Args: None.

Returns: None. (Modifies the global $ua variable).

=cut
sub init_user_agent {
    $ua = LWP::UserAgent->new;
    $ua->timeout(DEFAULT_TIMEOUT); # Set a timeout for HTTP requests to prevent indefinite hangs.
}

=head2 fetch_model_data(api_key)

Fetches metadata for the specified Gemini model from the API.

Args:
    $api_key (string): Your Google Gemini API key.

Returns:
    A hash reference containing the decoded JSON response with model metadata.

Croaks:
    If the API request fails for any reason (e.g., network error, invalid API key,
    API returns an error status).

=cut
sub fetch_model_data {
    my ($api_key) = @_; # Retrieve API key passed as an argument.

    say "\n--- Fetching Default Model Data ---";

    # Construct the URI for the model endpoint and add the API key as a query parameter.
    my $uri = URI->new(GEMINI_MODEL_ENDPOINT);
    $uri->query_form( key => $api_key ); # Adds '?key=YOUR_API_KEY' to the URI.

    # Send a GET request to the model endpoint.
    my $response = $ua->get($uri);

    # Check if the HTTP request was successful.
    unless ($response->is_success) {
        croak "Failed to get model data: " . $response->status_line . " - " . $response->content . "\n";
    }

    # Decode the JSON content from the response body into a Perl hash reference.
    return decode_json($response->content);
}

=head2 display_model_data(model_data)

Displays the fetched model metadata in a human-readable format.

Args:
    $model_data (hash reference): A hash reference containing the model metadata
        as returned by L<fetch_model_data>.

Returns: None. (Prints output to STDOUT).

=cut
sub display_model_data {
    my ($model_data) = @_; # Retrieve the hash reference passed as an argument.

    say "\n--- Default Model Data ---";
    # Iterate through the keys of the model data hash, sorted alphabetically for consistent output.
    foreach my $key (sort keys %$model_data) {
        my $value = $model_data->{$key};
        # Special handling for 'supportedGenerationMethods' which is an array of strings.
        # This joins the array elements with a comma and space for a cleaner display.
        if ($key eq "supportedGenerationMethods" && ref($value) eq 'ARRAY') {
            say "$key: " . join(', ', @$value);
        } else {
            # For all other keys, print the key and its scalar value.
            say "$key: $value";
        }
    }
}

=head2 generate_content(%params)

Sends a content generation request to the Gemini API.

This function constructs the JSON payload for the API request based on the
provided parameters and sends a POST request.

Args:
    %params (hash): A hash of parameters for the content generation. Expected keys:
        *   C<api_key> (string, required): Your Google Gemini API key.
        *   C<text> (string, required): The main text prompt for the LLM.
        *   C<instruction> (string, optional): System instruction for the LLM's persona.
            Defaults to "You are a helpful AI assistant.".
        *   C<temperature> (number, optional): Controls the randomness of the output.
            Defaults to 1.
        *   C<topP> (number, optional): The nucleus sampling parameter. Defaults to 0.95.
        *   C<topK> (number, optional): The top-k sampling parameter. Defaults to 64.
        *   C<thinking_budget> (number, optional): Budget for internal thinking steps.
            Defaults to -1 (unlimited).
        *   C<include_thoughts> (boolean, optional): Whether to include internal thoughts
            in the response. Defaults to 0 (false).
        *   C<enable_grounding> (boolean, optional): Whether to enable Google Search grounding.
            Defaults to 0 (false).

Returns:
    A hash reference containing the decoded JSON response from the API.

Croaks:
    If required parameters are missing (e.g., 'api_key', 'text') or if the
    API request fails.

=cut
sub generate_content {
    my (%params) = @_; # Accept parameters as a hash, allowing for named arguments.

    # Extract required and optional parameters using the defined-or operator (//).
    # This idiom provides default values if the parameter is undefined.
    # 'delete $params{key}' also removes the key from the %params hash,
    # which can be useful if you later want to check for unhandled parameters.
    my $api_key          = delete $params{api_key}          // croak "API key required for generate_content";
    my $text             = delete $params{text}             // croak "Missing 'text' parameter.";
    my $instruction      = delete $params{instruction}      // "You are a helpful AI assistant.";
    my $temperature      = delete $params{temperature}      // 1;
    my $topP             = delete $params{topP}             // 0.95;
    my $topK             = delete $params{topK}             // 64;
    my $thinking_budget  = delete $params{thinking_budget}  // -1;
    my $include_thoughts = delete $params{include_thoughts} // 0;
    my $enable_grounding = delete $params{enable_grounding} // 0; # Perl uses 0 for false, 1 for true.

    say "Sending Request...";

    # Construct the request payload as a Perl hash reference.
    # This structure directly maps to the JSON format expected by the Gemini API.
    my $request_payload = {
        system_instruction => {
            parts => [
                {
                    text => $instruction
                }
            ]
        },
        contents => [
            {
                parts => [
                    { text => $text }
                ]
            }
        ],
        generationConfig => {
            temperature => $temperature,
            topP => $topP,
            topK => $topK,
            thinkingConfig => {
                thinkingBudget => $thinking_budget,
                includeThoughts => $include_thoughts
            }
        },
        # Tools configuration, specifically for Google Search grounding.
        tools => [
            {
                google_search => {}
            }
        ]
    };

    # Conditionally remove the 'tools' section if grounding is not enabled.
    # This prevents sending unnecessary grounding configuration to the API.
    if (!$enable_grounding) {
        delete $request_payload->{tools} if exists $request_payload->{tools};
    }

    # Encode the Perl hash reference into a JSON string for the request body.
    my $json_payload = encode_json($request_payload);

    # Create an HTTP::Request object for a POST request.
    my $request = HTTP::Request->new(POST => GEMINI_API_ENDPOINT);
    # Set the 'Content-Type' header to indicate that the request body is JSON.
    $request->content_type('application/json');
    # Set the 'x-goog-api-key' header with the API key.
    $request->header('x-goog-api-key' => $api_key);
    # Set the JSON payload as the content (body) of the request.
    $request->content($json_payload);

    # Send the request using the LWP::UserAgent object.
    my $response = $ua->request($request);

    # Check if the HTTP response was successful.
    unless ($response->is_success) {
        croak "Failed to get response: " . $response->status_line . " - " . $response->content . "\n";
    }

    # Decode the JSON response body into a Perl hash reference.
    return decode_json($response->content);
}

=head2 display_response(response_data, %request_params)

Displays the generated content from the Gemini API response,
along with any associated grounding information and a summary of
the request parameters.

Args:
    $response_data (hash reference): The decoded JSON response from the API.
    %request_params (hash): The original parameters used for the request,
        used here for displaying a summary.

Returns: None. (Prints output to STDOUT).

Croaks:
    If the expected content structure (e.g., candidates, parts) is missing
    from the API response.

=cut
sub display_response {
    my ($response_data, %request_params) = @_;

    say "\n--- Response Data ---";

    # Access the generated text parts from the response data.
    # This path might vary slightly based on 'include_thoughts' setting.
    # The 'or croak' ensures the script stops if the expected data structure isn't found.
    my $generated_parts = $response_data->{candidates}[0]->{content}->{parts}
        or croak "No content parts found in response. Response structure: " . encode_json($response_data) . "\n";

    say "Generated Text:\n";
    # Iterate through each part of the generated content.
    # Each part can contain text, function calls, or other content.
    foreach my $part (@$generated_parts) {
        if (exists $part->{text}) {
            say $part->{text};
            say "-" x 50; # Optional separator for clarity between different text parts.
        }
    }

    # Check if grounding metadata is present in the response.
    if (exists $response_data->{candidates}[0]->{groundingMetadata}) {
        my $grounding_metadata = $response_data->{candidates}[0]->{groundingMetadata};

        say "\n--- Google Grounding Info ---";

        # Print search queries if available.
        if (exists $grounding_metadata->{webSearchQueries}) {
            say "Search Queries:";
            foreach my $query (@{$grounding_metadata->{webSearchQueries}}) {
                say "  - $query";
            }
        }

        # Print source links (URIs) if available.
        if (exists $grounding_metadata->{groundingChunks}) {
            say "\nSource Links:";
            foreach my $chunk (@{$grounding_metadata->{groundingChunks}}) {
                if (exists $chunk->{web} && exists $chunk->{web}->{uri}) {
                    say "  - " . $chunk->{web}->{uri};
                }
            }
        }
        say "-" x 50;
    }

    # Summarize the input parameters used for the request.
    say "\nSummary of values set for this request";
    say "---------------------------------------";
    foreach my $key (sort keys %request_params) {
        # Ensure boolean values (0/1) are displayed as 'false'/'true' for readability if desired,
        # otherwise print as their numeric value.
        my $display_value = $request_params{$key};
        if ($key eq 'include_thoughts' || $key eq 'enable_grounding') {
            $display_value = $display_value ? 'true' : 'false';
        }
        say "$key: " . $display_value;
    }
    say "---------------------------------------";
}

# --- Main script execution flow ---
# The 'main' subroutine orchestrates the overall logic of the script.
sub main {
    # Initialize the LWP::UserAgent object for making HTTP requests.
    init_user_agent();
    # Retrieve and validate the API key from environment variables.
    my $api_key = get_api_key();

    # --- Section 1: Getting the Model Data ---
    # Fetch metadata about the Gemini model.
    my $model_data = fetch_model_data($api_key);
    # Display the fetched model data.
    display_model_data($model_data);

    # Pause execution to allow the user to review the model data before proceeding.
    say "\nPress Enter to continue\n";
    <STDIN>; # Reads a line from standard input, pausing the script.

    # --- Section 2: Requesting a Response from the LLM ---
    # Define the parameters for the content generation request in a hash.
    # This makes it easy to modify or add new parameters.
    my %request_settings = (
        api_key          => $api_key, # Pass the retrieved API key.
        text             => "Explain the concept of LLM embeddings in simple terms.",
        instruction      => "You are a helpful AI assistant with a flourishing mindset",
        temperature      => 1,
        topP             => 0.95,
        topK             => 64,
        thinking_budget  => -1,
        include_thoughts => 0, # Set to 1 to see intermediate thoughts (if supported by model/API version).
        enable_grounding => 0, # Set to 1 to enable Google Search grounding.
    );

    # Generate content using the defined request settings.
    my $response_data = generate_content(%request_settings);
    # Display the generated response and a summary of the request parameters.
    display_response($response_data, %request_settings);
}

# Invoke the main subroutine to start the script execution.
main();