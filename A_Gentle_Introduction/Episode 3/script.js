// script.js
async function generateAndSendPlan() {
    const subject = document.getElementById("learning-input").value.trim();

    if (!subject) {
        alert("Please enter a subject.");
        return;
    }

    const plan = `I am eager to learn about ${subject} from scratch. I have 30 days and can dedicate 40 minutes each day to deliberate learning. Can you create a comprehensive 30-day learning plan for me to master the fundamentals and get hands-on practice with ${subject}? (for programming languages do not include code)`;

    document.getElementById("response").innerHTML = "";
    document.getElementById("prompt-display").textContent = "Prompt sent to LLM:\n\n" + plan;

    await sendRequest(plan);
}

async function sendDirectPrompt() {
    const prompt = document.getElementById("direct-input").value.trim();

    if (!prompt) {
        alert("Please enter a direct prompt.");
        return;
    }

    document.getElementById("response").innerHTML = "";
    document.getElementById("prompt-display").textContent = "Prompt sent to LLM:\n\n" + prompt;

    await sendRequest(prompt);
}

async function sendRequest(prompt) {
    const GEMINI_API_KEY = "YOUR_GEMINI_API_KEY_HERE"; // Replace with your API key
    const MODEL = "gemini-2.0-flash-exp";
    const url = `https://generativelanguage.googleapis.com/v1beta/models/${MODEL}:generateContent?key=${GEMINI_API_KEY}`;
    const headers = { "Content-Type": "application/json" };
    const data = { contents: [{ parts: [{ text: prompt }] }] };

    try {
        const response = await fetch(url, {
            method: "POST",
            headers: headers,
            body: JSON.stringify(data)
        });

        if (!response.ok) {
            throw new Error(`HTTP error! Status: ${response.status}`);
        }

        const json = await response.json();
        const textContent = json.candidates[0].content.parts[0].text;

        document.getElementById("response").textContent = textContent; // Directly set the response text
    } catch (error) {
        document.getElementById("response").textContent = `Error: ${error.message}`;
    }
}