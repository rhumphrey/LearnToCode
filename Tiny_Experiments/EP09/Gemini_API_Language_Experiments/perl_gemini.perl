use strict;
use warnings;
use LWP::UserAgent;
use JSON;
use URI; # Add this line
# use Data::Dumper; # Only needed for debugging

# It is highly recommended to set your API key as an environment variable
my $API_KEY = $ENV{"GEMINI_API_KEY"};

# --- API Key Check (keep this for robustness) ---
unless (defined $API_KEY && length $API_KEY > 0) {
    die "Error: GEMINI_API_KEY environment variable is not set or is empty. Please set it before running.\n" .
        "Example for Linux/macOS: export GEMINI_API_KEY=\"YOUR_API_KEY\"\n" .
        "Example for Windows: set GEMINI_API_KEY=YOUR_API_KEY\n";
}
# --- END API Key Check ---

my $GEMINI_API_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent";
my $GEMINI_MODEL_ENDPOINT = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash";

my $ua = LWP::UserAgent->new;
$ua->timeout(120); # Set a timeout for requests

# Getting the Model Data section

print "\n--- Default Model Data Before Any Changes ---\n";

my $uri = URI->new($GEMINI_MODEL_ENDPOINT);
$uri->query_form( key => $API_KEY );

my $model_response = $ua->get($uri);

if ($model_response->is_success) {
    my $model_data = decode_json($model_response->content);
    # Print the model metadata
    foreach my $key (sort keys %$model_data) {
        # --- THIS IS THE CORRECT PERL CHANGE ---
        if ($key eq "supportedGenerationMethods") {
            my $value = $model_data->{$key};
            # Check if the value is indeed an array reference
            if (ref($value) eq 'ARRAY') {
                print "$key: " . join(', ', @$value) . "\n";
            } else {
                # Fallback if it's not an array for some reason
                print "$key: $value\n";
            }
        } else {
            # For all other keys, print as usual
            print "$key: $model_data->{$key}\n";
        }
        # --- END OF CORRECT PERL CHANGE ---
    }
} else {
    die "Failed to get model data: " . $model_response->status_line . " - " . $model_response->content . " at " . __FILE__ . " line " . __LINE__ . ".\n";
}

print "\nPress any key to continue\n";
<STDIN>; # Read input to pause execution

# Requesting a response section

my $text = "Explain the concept of LLM embeddings in simple terms.";
my $instruction = "You are a helpful AI assistant with a flourishing mindset";
my $temperature = 1;
my $topP = 0.95;
my $topK = 64;
my $thinking_budget = -1;
my $include_thoughts = 0; # Perl uses 0/1 for boolean false/true
my $enable_grounding = 0;

# Define headers as a flat list (array reference) of key-value pairs (still valid, but we'll use it differently)
my $headers_list = [
    "x-goog-api-key", $API_KEY,
    "Content-Type", "application/json"
];


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

if ($enable_grounding) {
    # Keep this logic as is
} else {
    delete $request_payload->{tools} if exists $request_payload->{tools};
}


# Send the POST request
print "Sending Request...\n";
my $json_payload = encode_json($request_payload);

# --- NEW/CHANGED POST REQUEST SECTION ---
# Create an HTTP::Request object
my $request = HTTP::Request->new(POST => $GEMINI_API_ENDPOINT);

# Set the Content header
$request->content_type('application/json');

# Set the x-goog-api-key header directly
$request->header('x-goog-api-key' => $API_KEY);

# Set the JSON payload as the content
$request->content($json_payload);

# Send the request using the user agent
my $response = $ua->request($request);
# --- END NEW/CHANGED POST REQUEST SECTION ---


# Parse the JSON response body into a Perl hash
my $response_data;
if ($response->is_success) {
    $response_data = decode_json($response->content);
} else {
    die "Failed to get response: " . $response->status_line . " - " . $response->content . "\n";
}

# Extract the generated text from the structured response
my $generated_parts = $response_data->{candidates}[0]->{content}->{parts};

print "\n--- Response Data ---\n";

# Deal with potentially multiple parts of text (affected by 'include thoughts' setting)
print "Generated Text:\n\n";
foreach my $part (@$generated_parts) {
    if (exists $part->{text}) {
        print $part->{text} . "\n";
        print "-" x 50 . "\n"; # Optional: Add a separator for clarity between different text parts
    }
}

# Check if grounding metadata is present in the response
if (exists $response_data->{candidates}[0]->{groundingMetadata}) {
    my $grounding_metadata = $response_data->{candidates}[0]->{groundingMetadata};

    print "\n--- Google Grounding Info ---\n";

    # Print search queries
    if (exists $grounding_metadata->{webSearchQueries}) {
        print "Search Queries:\n";
        foreach my $query (@{$grounding_metadata->{webSearchQueries}}) {
            print "  - $query\n";
        }
    }

    # Print source links
    if (exists $grounding_metadata->{groundingChunks}) {
        print "\nSource Links:\n";
        foreach my $chunk (@{$grounding_metadata->{groundingChunks}}) {
            if (exists $chunk->{web} && exists $chunk->{web}->{uri}) {
                print "  - " . $chunk->{web}->{uri} . "\n";
            }
        }
    }
    print "-" x 50 . "\n";
}

print "\nSummary of values set for this request\n";
print "---------------------------------------\n";
print "text: " . $text . "\n";
print "instruction: " . $instruction . "\n";
print "temperature: " . $temperature . "\n";
print "topP: " . $topP . "\n";
print "topK: " . $topK . "\n";
print "thinking_budget: " . $thinking_budget . "\n";
print "include_thoughts: " . $include_thoughts . "\n";
print "enable_grounding: " . $enable_grounding . "\n";
print "---------------------------------------\n";