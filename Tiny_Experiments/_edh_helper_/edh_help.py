import os
import requests
import datetime
import json
import re
from dataclasses import dataclass
from typing import Optional, Tuple, List, Dict, Any

# --- Configuration ---
GEMINI_API_KEY = os.getenv("GEMINI_API_KEY")
GEMINI_MODEL_NAME = "gemini-2.5-flash"
COMMANDER_DECKS_FOLDER = "commander_decks"
GEMINI_API_BASE_URL = "https://generativelanguage.googleapis.com/v1beta/models"

@dataclass
class GenerationParameters:
    temperature: float
    top_p: float
    top_k: int

@dataclass
class DeckBuildingConstraints:
    era: Optional[str] = None
    budget: Optional[str] = None
    niche_strategies: Optional[str] = None
    combo_inclusion: Optional[str] = None
    favorite_card: Optional[str] = None

@dataclass
class AppConfig:
    generation_params: GenerationParameters

def create_deck_building_prompt(user_input: str, constraints: DeckBuildingConstraints, build_strategy: Optional[str] = None) -> str:
    """Creates a prompt for building a Commander deck based on user input and constraints."""
    
    # Base prompt structure
    prompt_parts = [
        "You are an expert Magic: The Gathering EDH/Commander deck builder.",
        "Create a comprehensive Commander deck based on the following input and constraints.",
        "Remember the Commander format rules: 100-card singleton deck, color identity restrictions, 40 life, 21 commander damage rule.",
        "",
        f"USER INPUT: {user_input}",
        "",
        "CONSTRAINTS:"
    ]
    
    # Add constraints if provided
    if constraints.era:
        prompt_parts.append(f"- Era/Set restrictions: {constraints.era}")
    if constraints.budget:
        prompt_parts.append(f"- Budget: {constraints.budget}")
    if constraints.niche_strategies:
        prompt_parts.append(f"- Niche strategies: {constraints.niche_strategies}")
    if constraints.combo_inclusion:
        prompt_parts.append(f"- Combo preference: {constraints.combo_inclusion}")
    if constraints.favorite_card:
        prompt_parts.append(f"- Favorite card to include: {constraints.favorite_card}")
    
    if not any([constraints.era, constraints.budget, constraints.niche_strategies, 
                constraints.combo_inclusion, constraints.favorite_card]):
        prompt_parts.append("- No specific constraints provided")
    
    prompt_parts.append("")
    
    if build_strategy:
        prompt_parts.append("BUILD STRATEGY:")
        prompt_parts.append(build_strategy)
        prompt_parts.append("")
        prompt_parts.append("Based on the above strategy, provide a complete 100-card Commander decklist.")
    else:
        prompt_parts.append("First, provide a build strategy for this deck considering the Commander format rules and constraints.")
        prompt_parts.append("Then, provide a complete 100-card Commander decklist.")
    
    prompt_parts.append("")
    prompt_parts.append("DECKLIST FORMAT:")
    prompt_parts.append("- Organize by card category: Ramp, Card Draw, Removal, Board Wipes, Synergy Pieces, Win Conditions, etc.")
    prompt_parts.append("- Include approximately 36-38 lands appropriate for the color identity")
    prompt_parts.append("- Ensure the deck follows singleton format rules (only basic lands can be repeated)")
    prompt_parts.append("- Make sure all cards fit within the commander's color identity")
    prompt_parts.append("")
    
    if not build_strategy:
        prompt_parts.append("After the decklist, provide a player's guide with:")
        prompt_parts.append("- Overall strategy and win conditions")
        prompt_parts.append("- Key synergies and combo explanations")
        prompt_parts.append("- Mulligan guidelines")
        prompt_parts.append("- Common matchup considerations and sideboard suggestions")
        prompt_parts.append("- Potential upgrades or budget alternatives")
    
    return "\n".join(prompt_parts)

def create_validation_prompt(decklist: str, user_input: str, constraints: DeckBuildingConstraints) -> str:
    """Creates a prompt for validating a Commander decklist."""
    return f"""
You are an expert Magic: The Gathering EDH/Commander deck validator. 
Please validate the following decklist and provide feedback.

USER INPUT: {user_input}

CONSTRAINTS:
- Era/Set restrictions: {constraints.era or 'None'}
- Budget: {constraints.budget or 'None'}
- Niche strategies: {constraints.niche_strategies or 'None'}
- Combo preference: {constraints.combo_inclusion or 'None'}
- Favorite card: {constraints.favorite_card or 'None'}

DECKLIST TO VALIDATE:
{decklist}

Please provide:
1. Validation of format compliance (100 cards, singleton, color identity)
2. Assessment of mana curve and land count
3. Evaluation of card synergy with the commander/strategy
4. Analysis of constraint adherence (budget, era, etc.)
5. Recommendations for improvements
6. Sideboard suggestions for common matchups
7. A player's guide with:
   - Overall strategy and win conditions
   - Key synergies and combo explanations
   - Mulligan guidelines
   - Common matchup considerations
   - Potential upgrades or budget alternatives
"""

def extract_llm_response_text(gemini_response: dict) -> str:
    """Extracts the main text content from the Gemini API response."""
    try:
        candidate = gemini_response['candidates'][0]
        return candidate['content']['parts'][0]['text']
    except (KeyError, IndexError):
        return "No relevant content found or response was not in expected format."

def create_api_payload(prompt_text: str, generation_params: GenerationParameters) -> dict:
    """Creates the payload for the Gemini API request."""
    return {
        "contents": [{"parts": [{"text": prompt_text}]}],
        "generationConfig": {
            "temperature": generation_params.temperature,
            "topP": generation_params.top_p,
            "topK": generation_params.top_k,
            "thinkingConfig": {"thinkingBudget": -1}
        },
        "safetySettings": [
            {"category": "HARM_CATEGORY_HARASSMENT", "threshold": "BLOCK_NONE"},
            {"category": "HARM_CATEGORY_HATE_SPEECH", "threshold": "BLOCK_NONE"},
            {"category": "HARM_CATEGORY_SEXUALLY_EXPLICIT", "threshold": "BLOCK_NONE"},
            {"category": "HARM_CATEGORY_DANGEROUS_CONTENT", "threshold": "BLOCK_NONE"},
        ],
        "tools": [{"google_search": {}}],
    }

def make_api_request(prompt_text: str, generation_params: GenerationParameters) -> Tuple[Optional[dict], Optional[str]]:
    """
    Sends the prompt to the Google Gemini API.
    Returns a tuple of (response, error_message)
    """
    if not GEMINI_API_KEY:
        return None, "API key not found. Please set the GEMINI_API_KEY environment variable."

    endpoint = f"{GEMINI_API_BASE_URL}/{GEMINI_MODEL_NAME}:generateContent"
    headers = {"Content-Type": "application/json"}
    payload = create_api_payload(prompt_text, generation_params)

    try:
        response = requests.post(
            endpoint, 
            headers=headers, 
            json=payload, 
            params={"key": GEMINI_API_KEY},
            timeout=300
        )
        response.raise_for_status()
        return response.json(), None
    except requests.exceptions.RequestException as err:
        error_msg = f"API error occurred: {err}"
        try:
            if response is not None and hasattr(response, 'text'):
                error_details = json.loads(response.text)
                if 'error' in error_details and 'message' in error_details['error']:
                    error_msg += f"\nAPI Error Details: {error_details['error']['message']}"
                else:
                    error_msg += f"\nAPI Raw Error Response: {json.dumps(error_details, indent=2)}"
            elif hasattr(response, 'text'):
                error_msg += f"\nAPI Raw Error Response: {response.text}"
        except (json.JSONDecodeError, AttributeError):
            pass
        return None, error_msg

def ensure_directory_exists(folder: str) -> None:
    """Ensures the output directory exists, avoiding race conditions."""
    try:
        os.makedirs(folder, exist_ok=True)
    except OSError as e:
        print(f"Error creating directory {folder}: {e}")

def write_to_file(folder: str, content: str, filename_prefix: str) -> None:
    """Saves content to a timestamped Markdown file."""
    ensure_directory_exists(folder)
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(folder, f"{filename_prefix}_{timestamp}.md")

    try:
        with open(filename, "w", encoding="utf-8") as f:
            f.write(content)
        print(f"Deck saved to: {filename}")
    except IOError as e:
        print(f"Error saving file: {e}")

def get_user_input() -> Tuple[str, DeckBuildingConstraints]:
    """Gets user input for deck building."""
    print("Welcome to the EDH/Commander Deck Builder!")
    print("Please provide information about the deck you want to create.")
    
    # Get primary input
    print("\nChoose your primary building direction:")
    print("1. Build around a specific Commander")
    print("2. Build around a specific card")
    print("3. Build around a color combination")
    print("4. Build around an archetype")
    
    choice = input("Enter your choice (1-4): ").strip()
    
    if choice == "1":
        user_input = input("Enter the Commander you want to build around: ").strip()
    elif choice == "2":
        user_input = input("Enter the card you want to build around: ").strip()
    elif choice == "3":
        user_input = input("Enter the color or color combination (e.g., 'Red Green', 'Selesnya', 'Grixis'): ").strip()
    elif choice == "4":
        user_input = input("Enter the archetype (e.g., 'Tokens', 'Control', 'Reanimator', 'Voltron'): ").strip()
    else:
        print("Invalid choice. Defaulting to building around a Commander.")
        user_input = input("Enter the Commander you want to build around: ").strip()
    
    # Get optional constraints
    print("\nNow provide any additional constraints (press Enter to skip any):")
    
    era = input("Era/Set restrictions (e.g., 'Only cards from Innistrad block', 'No cards before 2015'): ").strip()
    budget = input("Budget (e.g., 'Budget $50', 'No budget restrictions'): ").strip()
    niche_strategies = input("Niche strategies (e.g., 'Group hug', 'Aristocrats', 'Landfall'): ").strip()
    combo_inclusion = input("Combo preference (e.g., 'Include infinite combos', 'No combos', 'Only 2-card combos'): ").strip()
    favorite_card = input("Favorite card to include (e.g., 'Smothering Tithe', 'Cyclonic Rift'): ").strip()
    
    constraints = DeckBuildingConstraints(
        era=era if era else None,
        budget=budget if budget else None,
        niche_strategies=niche_strategies if niche_strategies else None,
        combo_inclusion=combo_inclusion if combo_inclusion else None,
        favorite_card=favorite_card if favorite_card else None
    )
    
    return user_input, constraints

def create_commander_deck(user_input: str, constraints: DeckBuildingConstraints, app_config: AppConfig) -> None:
    """Creates a Commander deck based on user input and constraints."""
    if not GEMINI_API_KEY:
        print("Error: GEMINI_API_KEY environment variable is not set.")
        print("Please set it before running the application.")
        return

    # Step 1: Generate build strategy
    print("Generating build strategy...")
    strategy_prompt = create_deck_building_prompt(user_input, constraints)
    gemini_response, error_message = make_api_request(strategy_prompt, app_config.generation_params)
    
    if error_message:
        print(f"API request failed: {error_message}")
        return
        
    if not gemini_response:
        print("Failed to get valid response from Gemini API for strategy.")
        return
    
    strategy_response = extract_llm_response_text(gemini_response)
    
    # Step 2: Generate decklist using the strategy
    print("Generating decklist...")
    decklist_prompt = create_deck_building_prompt(user_input, constraints, strategy_response)
    gemini_response, error_message = make_api_request(decklist_prompt, app_config.generation_params)
    
    if error_message:
        print(f"API request failed: {error_message}")
        return
        
    if not gemini_response:
        print("Failed to get valid response from Gemini API for decklist.")
        return
    
    decklist_response = extract_llm_response_text(gemini_response)
    
    # Step 3: Validate deck and generate player's guide
    print("Validating deck and creating player's guide...")
    validation_prompt = create_validation_prompt(decklist_response, user_input, constraints)
    gemini_response, error_message = make_api_request(validation_prompt, app_config.generation_params)
    
    if error_message:
        print(f"API request failed: {error_message}")
        return
        
    if not gemini_response:
        print("Failed to get valid response from Gemini API for validation.")
        return
    
    validation_response = extract_llm_response_text(gemini_response)
    
    # Combine all responses
    full_response = f"""
# EDH/Commander Deck Building Result

## Original Input
- **Build Around**: {user_input}
- **Era/Set Restrictions**: {constraints.era or 'None'}
- **Budget**: {constraints.budget or 'None'}
- **Niche Strategies**: {constraints.niche_strategies or 'None'}
- **Combo Preference**: {constraints.combo_inclusion or 'None'}
- **Favorite Card**: {constraints.favorite_card or 'None'}

## Build Strategy
{strategy_response}

## Decklist
{decklist_response}

## Validation and Player's Guide
{validation_response}

---
*Generated on {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")} using Gemini {GEMINI_MODEL_NAME}*
"""
    
    # Save results
    filename_prefix = re.sub(r'[^a-zA-Z0-9]', '_', user_input.lower())[:20]
    write_to_file(COMMANDER_DECKS_FOLDER, full_response, filename_prefix)
    print("Deck created successfully!")

def main():
    """Main function for the Commander deck building application."""
    # Default generation parameters
    app_config = AppConfig(
        generation_params=GenerationParameters(
            temperature=0.8,  # Slightly higher temperature for creativity
            top_p=0.95,
            top_k=64
        )
    )
    
    # Get user input
    user_input, constraints = get_user_input()
    
    if user_input:
        create_commander_deck(user_input, constraints, app_config)
    else:
        print("No input provided. Please try again.")

if __name__ == "__main__":
    main()