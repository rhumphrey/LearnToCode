# How to Install Qodo (formerly CodiumAI) in VS Code

This guide provides step-by-step instructions on how to install and set up the Qodo extension (formerly known as CodiumAI) in Visual Studio Code. Qodo is an AI-powered tool that helps developers write more reliable code by generating tests, providing code suggestions, and assisting with code reviews.

## Prerequisites

* **Visual Studio Code:** Make sure you have VS Code installed on your system. If not, download it from [https://code.visualstudio.com/](https://code.visualstudio.com/).
* **Internet Connection:** Qodo requires an active internet connection to communicate with its AI models.

## Installation Steps

Follow these steps to install the Qodo Gen extension:

### Step 1: Open VS Code

Launch Visual Studio Code on your computer.

### Step 2: Access the Extensions View

In VS Code, navigate to the Extensions view. You can do this in a few ways:

* Click on the **Extensions icon** in the Activity Bar on the side of the window (it looks like four squares, with one flying off).
* Alternatively, press the keyboard shortcut: `Ctrl+Shift+X` (Windows/Linux) or `Cmd+Shift+X` (macOS).

### Step 3: Search for "Qodo Gen"

In the search bar at the top of the Extensions view, type **"Qodo Gen"**.

* You might also find it by searching "CodiumAI" initially, but "Qodo Gen" is the current name.
* Look for the official extension published by `Qodo`.

### Step 4: Install the Extension

Once you find the "Qodo Gen" extension in the search results, click the **"Install"** button next to it.

* VS Code will download and install the extension.

### Step 5: Sign In / Authorize Qodo

After the installation is complete, you'll need to authorize Qodo to use its AI features.

* **Look for a notification:** A notification usually pops up in the bottom-right corner of VS Code prompting you to sign in. Click the "Sign in" button within this notification.
* **Follow the browser redirect:** This action will open your default web browser and redirect you to Qodo's authentication page.
* **Log in:** Choose your preferred method to log in (e.g., Google or GitHub account, or create a new Qodo account).
* **Return to VS Code:** Once you've successfully logged in through the browser, you'll typically be redirected back to VS Code. A status message (often in the bottom status bar or a small popup) should confirm that you are logged in and Qodo is ready.
* **If no prompt appears:** If you don't see the sign-in prompt, look for the "Qodo" icon or text in the **VS Code Status Bar** at the very bottom. Clicking this often brings up options, including one to log in or refresh your connection.

### Step 6: Start Using Qodo

Once installed and authorized, Qodo will integrate into your workflow.

* Open any of your Python, JavaScript, TypeScript, or Java code files.
* You'll typically see **"Generate Tests"** buttons or code lenses appearing above functions and classes.
* You can also right-click on code selections to access Qodo's context-aware features like "Explain Code," "Improve Code," or to interact with the Qodo chat.
* A dedicated Qodo sidebar panel might also be available for direct interaction and settings.

## Troubleshooting Tips

* **Restart VS Code:** If Qodo doesn't seem to be working after installation, try closing and reopening VS Code.
* **Check Output Panel:** For any errors, go to `View > Output` and select "Qodo Gen" from the dropdown menu to see logs.
* **Internet Connection:** Ensure your internet connection is stable.
* **Firewall/Proxy:** If you're on a corporate network, ensure your firewall or proxy isn't blocking Qodo's access to its servers.
