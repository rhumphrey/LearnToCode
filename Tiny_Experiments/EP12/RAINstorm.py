import requests
import itertools
import time
import os

# === 1. Prompt Input ===
# Some prompts to try
# "I notice tension in my shoulders. Where might this tension be coming from?"
# "I keep worrying about tomorrow. What questions can I ask to understand this worry?"
# "I feel emotionally numb lately. How can I begin to reconnect with what I'm feeling?"
# "There's a tightness in my chest when I think about work. What might this be trying to tell me?"
# "I keep replaying a mistake I made last week. How do I allow this feeling without getting stuck in it?"
# "I feel disconnected from people I care about. What might be happening beneath that feeling?"
# "I often feel like I'm not doing enough. What questions could help me investigate this belief?"
# "When I sit still, I feel a subtle sense of dread. How can I explore this gently?"
# "I notice a resistance to self-compassion. What does that resistance need in order to soften?"
# "I feel overwhelmed by choices. How can I recognize and allow this without rushing to fix it?"
user_prompt = input("Enter the user prompt to test: ").strip()
prompt_key = "custom_prompt"

# === 2. Models to Test ===
models = {
    "phi3":         "Phi3 [Mini] (3.8B)",
    "llama3.2":     "LLaMA 3.2 [Mini] (3B)",
    "qwen2.5:3b":   "Qwen 2.5 (3B)",
    "llama3.2:1b":  "LLaMA 3.2 [Mini] (1B)",
    "qwen2.5:1.5b": "Qwen 2.5 (1.5B)",
    "qwen2.5:0.5b": "Qwen 2.5 (0.5B)" 
}

# === 3. System Instructions ===
system_instructions = {
    "agnostic_baseline":        "You are a neutral assistant responding helpfully to any user request.",
    "gentle_coach":             "You are a gentle mindfulness coach guiding a user through RAIN (Recognize, Allow, Investigate, Nurture).",
    "curious_inquirer":         "You are a curious inquirer helping the user deepen self-investigation.",
    "guided_meditation_voice":  "You write as if leading a short, calming guided meditation, using soft, sensory language.",
    "inquiry_coach":            "You focus on asking gentle, inward-looking questions to deepen self-awareness in the RAIN (Recognize, Allow, Investigate, Nurture) framework.",
    "emotion_labeler":          "You help the user find precise emotion labels and related body sensations before offering reflection.",
    "socratic_guide":           "You are a Socratic teacher who only responds with probing, open-ended questions to help the user discover answers for themselves.",
    "warm_storyteller":         "You use analogies, short anecdotes, and vivid imagery to make the explanation emotionally engaging.",
    "critical_challenger":      "You politely challenge assumptions in the user's question and offer alternative perspectives.",
    "future_self_visualizer":   "You guide the user to imagine and describe their compassionate future self, using detailed sensory and emotional language to inspire the 'Nurture' step of RAIN (Recognize, Allow, Investigate, Nurture)."
}

# === 4. Sampling Parameters ===
generation_params = {
    "temperature": 0.7,
    "top_p": 0.9,
    "top_k": 50
}

# === 5. Ollama API Setup ===
BASE_URL = "http://localhost:11434"
ENDPOINT = "/v1/chat/completions"

# === 6. Unique Markdown filename per session ===
timestamp = time.strftime('%Y%m%d_%H%M%S')
MD_PATH  = f"rain_ab_test_results_{timestamp}.md"

# === 7. Markdown Logging ===
with open(MD_PATH, mode="w", encoding="utf-8") as mdfile:
    mdfile.write(f"# RAIN A/B Test Results\n\n")
    mdfile.write(f"**Prompt:** {user_prompt}\n\n")
    mdfile.write(f"_Generated on {time.strftime('%Y-%m-%d %H:%M:%S')}_\n\n")

    # Iterate Over All Combinations
    for (model_key, model_label), (sys_key, sys_inst) in itertools.product(models.items(), system_instructions.items()):
        print(f"→ Running {sys_key} with {model_label}...")
        payload = {
            "model": model_key,
            "messages": [
                {"role": "system", "content": sys_inst},
                {"role": "user",   "content": user_prompt}
            ],
            **generation_params
        }

        try:
            resp = requests.post(BASE_URL + ENDPOINT, json=payload)
            resp.raise_for_status()
            answer = resp.json()["choices"][0]["message"]["content"].strip()
        except Exception as e:
            answer = f"[ERROR] {str(e)}"

        mdfile.write(f"**Model:** {model_label}  \n\n")
        mdfile.write(f"**System Instruction:** `{sys_key}`\n\n")
        mdfile.write(f"**Instruction Text:** {sys_inst}\n\n")
        mdfile.write("**Response:**\n\n")
        mdfile.write("```markdown\n")
        mdfile.write(answer + "\n")
        mdfile.write("```\n\n")

        time.sleep(0.5)  # Optional: throttle requests

print(f"\n *** Experiment complete. Results saved to: {MD_PATH} ***")
