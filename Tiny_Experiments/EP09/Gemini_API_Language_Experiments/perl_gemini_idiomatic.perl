use strict;
use warnings;
use LWP::UserAgent;
use JSON qw(encode_json decode_json); # Explicitly import functions
use URI;
use Carp qw(croak); # For dying with file/line info from caller's perspective
use feature 'say'; # For say() function (automatically adds newline)

# --- Constants ---
# Use 'use constant' for values that should not change
use constant GEMINI_API_ENDPOINT   => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent";
use constant GEMINI_MODEL_ENDPOINT => "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash";
use constant DEFAULT_TIMEOUT       => 120;

# Global User Agent (initialized once)
my $ua;

# --- Subroutines for better organization and reusability ---

# Fetches and validates the API key from environment variables.
# Croaks if the key is not set or empty.
sub get_api_key {
    my $api_key = $ENV{"GEMINI_API_KEY"};
    unless (defined $api_key && length $api_key > 0) {
        croak "Error: GEMINI_API_KEY environment variable is not set or is empty.\n" .
              "Please set it before running.\n" .
              "Example for Linux/macOS: export GEMINI_API_KEY=\"YOUR_API_KEY\"\n" .
              "Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY\n";
    }
    return $api_key;
}

# Initializes the LWP::UserAgent object.
sub init_user_agent {
    $ua = LWP::UserAgent->new;
    $ua->timeout(DEFAULT_TIMEOUT);
}

# Fetches metadata for the specified Gemini model.
# Takes the API key as an argument.
# Returns a hash reference of the model data.
sub fetch_model_data {
    my ($api_key) = @_;

    say "\n--- Fetching Default Model Data ---";

    my $uri = URI->new(GEMINI_MODEL_ENDPOINT);
    $uri->query_form( key => $api_key );

    my $response = $ua->get($uri);

    unless ($response->is_success) {
        croak "Failed to get model data: " . $response->status_line . " - " . $response->content . "\n";
    }

    return decode_json($response->content);
}

# Displays the model data in a formatted way.
# Takes a hash reference of the model data as an argument.
sub display_model_data {
    my ($model_data) = @_;

    say "\n--- Default Model Data ---";
    foreach my $key (sort keys %$model_data) {
        my $value = $model_data->{$key};
        # Special handling for 'supportedGenerationMethods' which is an array
        if ($key eq "supportedGenerationMethods" && ref($value) eq 'ARRAY') {
            say "$key: " . join(', ', @$value);
        } else {
            say "$key: $value";
        }
    }
}

# Generates content using the Gemini API.
# Takes a hash of parameters including text, instruction, temperature, etc.
# Returns a hash reference of the API response data.
sub generate_content {
    my (%params) = @_; # Use a hash for named parameters

    # Extract required and optional parameters with default values
    my $api_key          = delete $params{api_key}          // croak "API key required for generate_content";
    my $text             = delete $params{text}             // croak "Missing 'text' parameter.";
    my $instruction      = delete $params{instruction}      // "You are a helpful AI assistant.";
    my $temperature      = delete $params{temperature}      // 1;
    my $topP             = delete $params{topP}             // 0.95;
    my $topK             = delete $params{topK}             // 64;
    my $thinking_budget  = delete $params{thinking_budget}  // -1;
    my $include_thoughts = delete $params{include_thoughts} // 0;
    my $enable_grounding = delete $params{enable_grounding} // 0;

    say "Sending Request...";

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
        tools => [
            {
                google_search => {}
            }
        ]
    };

    # Conditionally remove tools if grounding is not enabled
    if (!$enable_grounding) {
        delete $request_payload->{tools} if exists $request_payload->{tools};
    }

    my $json_payload = encode_json($request_payload);

    my $request = HTTP::Request->new(POST => GEMINI_API_ENDPOINT);
    $request->content_type('application/json');
    $request->header('x-goog-api-key' => $api_key);
    $request->content($json_payload);

    my $response = $ua->request($request);

    unless ($response->is_success) {
        croak "Failed to get response: " . $response->status_line . " - " . $response->content . "\n";
    }

    return decode_json($response->content);
}

# Displays the generated content and any associated grounding information.
# Takes the API response data and the original request parameters as arguments.
sub display_response {
    my ($response_data, %request_params) = @_;

    say "\n--- Response Data ---";

    # Handle cases where candidates might not be present or content structure varies
    my $generated_parts = $response_data->{candidates}[0]->{content}->{parts}
        or croak "No content parts found in response.";

    say "Generated Text:\n";
    foreach my $part (@$generated_parts) {
        if (exists $part->{text}) {
            say $part->{text};
            say "-" x 50; # Separator for clarity
        }
    }

    # Check and display grounding metadata
    if (exists $response_data->{candidates}[0]->{groundingMetadata}) {
        my $grounding_metadata = $response_data->{candidates}[0]->{groundingMetadata};

        say "\n--- Google Grounding Info ---";

        if (exists $grounding_metadata->{webSearchQueries}) {
            say "Search Queries:";
            foreach my $query (@{$grounding_metadata->{webSearchQueries}}) {
                say "  - $query";
            }
        }

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

    # Summarize the request parameters
    say "\nSummary of values set for this request";
    say "---------------------------------------";
    foreach my $key (sort keys %request_params) {
        say "$key: " . $request_params{$key};
    }
    say "---------------------------------------";
}

# --- Main script execution ---
sub main {
    init_user_agent(); # Initialize LWP::UserAgent
    my $api_key = get_api_key(); # Get and validate API key

    # Section 1: Getting Model Data
    my $model_data = fetch_model_data($api_key);
    display_model_data($model_data);

    say "\nPress Enter to continue\n";
    <STDIN>; # Pause execution

    # Section 2: Requesting a response
    my %request_settings = (
        api_key          => $api_key, # Pass API key to the function
        text             => "Explain the concept of LLM embeddings in simple terms.",
        instruction      => "You are a helpful AI assistant with a flourishing mindset",
        temperature      => 1,
        topP             => 0.95,
        topK             => 64,
        thinking_budget  => -1,
        include_thoughts => 0,
        enable_grounding => 0,
    );

    my $response_data = generate_content(%request_settings);
    display_response($response_data, %request_settings); # Pass settings for summary display
}

# Run the main subroutine
main();