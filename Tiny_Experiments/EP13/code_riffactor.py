import google.generativeai as genai
import os
import re
from pathlib import Path
import tempfile

class CodeRiffactor:
    def __init__(self):
        self.api_key = None
        self.model = None
        self.file_content = None
        self.language = None
        self.filename = None
        self.output_preference = "file"  # Default to saving files
        
    def setup_gemini(self):
        """Initialize the Gemini API"""
        try:
            # Try to get API key from environment variable
            self.api_key = os.environ.get("GEMINI_API_KEY")
            
            if not self.api_key:
                # Prompt user if not found in environment
                self.api_key = input("Key Not Found - Enter your Google Gemini API key: ").strip()
            
            genai.configure(api_key=self.api_key)
            
            # Initialize the model (using flash as requested)
            self.model = genai.GenerativeModel('gemini-2.5-flash')
            
            # Test the connection with a simple prompt
            test_response = self.model.generate_content("Hello")
            return True
        except Exception as e:
            print(f"Error setting up Gemini API: {e}")
            return False
    
    def set_output_preference(self):
        """Let user choose how to handle output"""
        print("\nOutput Options:")
        print("1. Save to files (default)")
        print("2. Print to console only")
        print("3. Both save to files and print to console")
        
        choice = input("Choose output method (1-3): ").strip()
        if choice == "2":
            self.output_preference = "console"
        elif choice == "3":
            self.output_preference = "both"
        else:
            self.output_preference = "file"
            
        print(f"Output set to: {self.output_preference}")
    
    def handle_output(self, content, filename):
        """Handle output based on user preference"""
        if self.output_preference in ["file", "both"]:
            # Add separation at the end of content
            content_with_separation = content + "\n\n"
            
            with open(filename, 'a', encoding='utf-8') as file:
                file.write(content_with_separation)
            print(f"Output saved to {filename}")
    
        if self.output_preference in ["console", "both"]:
            print("\n" + "="*60)
            print(f"CONTENT: {filename}")
            print("="*60)
            print(content)  # Note: console output doesn't get the extra newlines
            print("="*60)
    
    def load_file(self):
        """Prompt user for a file and load its contents"""
        while True:
            file_path = input("Enter the path to your code file: ").strip()
            
            if not os.path.exists(file_path):
                print("File not found. Please try again.")
                continue
                
            try:
                with open(file_path, 'r', encoding='utf-8') as file:
                    self.file_content = file.read()
                self.filename = Path(file_path).stem
                print(f"Successfully loaded: {file_path}")
                return True
            except Exception as e:
                print(f"Error reading file: {e}")
                return False
    
    def identify_language(self):
        """Identify the programming language of the loaded file"""
        if not self.file_content:
            print("No file loaded. Please load a file first.")
            return
        
        prompt = f"""Identify the programming language of the following code snippet.
        Return only the name of the language in your response.
        
        Code:
        {self.file_content[:2000]}  # Limit to first 2000 chars to avoid token limits
        """
        
        try:
            response = self.model.generate_content(prompt)
            self.language = response.text.strip()
            print(f"Identified language: {self.language}")
            return self.language
        except Exception as e:
            print(f"Error identifying language: {e}")
            return None
    
    def code_review(self):
        """Perform a thorough code review"""
        if not self.file_content:
            print("No file loaded. Please load a file first.")
            return
        
        prompt = f"""Perform a thorough code review of the following {self.language} code.
        Provide a comprehensive analysis including:
        1. Code quality assessment
        2. Potential bugs or issues
        3. Security vulnerabilities
        4. Performance considerations
        5. Best practices compliance
        6. Specific recommendations for improvement
        
        Format your response in Markdown with appropriate headings.
        
        Code:
        {self.file_content}
        """
        
        try:
            print("Performing code review...")
            response = self.model.generate_content(prompt)
            
            # Handle output based on preference
            output_content = f"# Code Review for {self.filename}\n\n{response.text}"
            self.handle_output(output_content, f"{self.filename}_review.md")
            
            return response.text
        except Exception as e:
            print(f"Error performing code review: {e}")
            return None
    
    def beginner_analysis(self):
        """Create a beginner-friendly explanation of the code"""
        if not self.file_content:
            print("No file loaded. Please load a file first.")
            return
        
        prompt = f"""Provide a step-by-step explanation of what the following {self.language} code does.
        The explanation should be suitable for a beginner programmer.
        Break down complex concepts and explain them in simple terms.
        Use analogies where helpful.
        
        Format your response in Markdown with appropriate headings.
        
        Code:
        {self.file_content}
        """
        
        try:
            print("Creating beginner-friendly analysis...")
            response = self.model.generate_content(prompt)
            
            # Handle output based on preference
            output_content = f"# Beginner's Guide to {self.filename}\n\n{response.text}"
            self.handle_output(output_content, f"{self.filename}_beginner_guide.md")
            
            return response.text
        except Exception as e:
            print(f"Error creating beginner analysis: {e}")
            return None
    
    def refactor_for_paradigms(self):
        """Refactor code into different programming paradigms"""
        if not self.file_content or not self.language:
            print("Please identify the language first (option 1).")
            return
        
        prompt = f"""For the programming language {self.language}, identify the various programming paradigms it supports.
        Then, refactor the following code to demonstrate different paradigms and their idioms in {self.language}.
        
        For each paradigm:
        1. Name the paradigm
        2. Briefly explain its characteristics
        3. Provide the refactored code using that paradigm
        4. Explain the changes made
        
        Format your response in Markdown with appropriate headings.
        
        Code:
        {self.file_content}
        """
        
        try:
            print("Refactoring for different paradigms...")
            response = self.model.generate_content(prompt)
            
            # Handle output based on preference
            output_content = f"# Paradigm Refactoring for {self.filename}\n\n{response.text}"
            self.handle_output(output_content, f"{self.filename}_paradigm_refactor.md")
            
            return response.text
        except Exception as e:
            print(f"Error refactoring for paradigms: {e}")
            return None
    
    def suggest_improvements(self):
        """Suggest general improvements to the code"""
        if not self.file_content:
            print("No file loaded. Please load a file first.")
            return
        
        prompt = f"""Suggest improvements for the following {self.language} code.
        Focus on:
        1. Readability and maintainability
        2. Performance optimizations
        3. Modern language features that could be applied
        4. Error handling improvements
        5. Code structure and organization
        
        Provide both the recommendations and refactored code examples.
        
        Format your response in Markdown with appropriate headings.
        
        Code:
        {self.file_content}
        """
        
        try:
            print("Generating improvement suggestions...")
            response = self.model.generate_content(prompt)
            
            # Handle output based on preference
            output_content = f"# Improvement Suggestions for {self.filename}\n\n{response.text}"
            self.handle_output(output_content, f"{self.filename}_improvements.md")
            
            return response.text
        except Exception as e:
            print(f"Error generating improvement suggestions: {e}")
            return None
    
    def generate_documentation(self):
        """Generate documentation for the code"""
        if not self.file_content:
            print("No file loaded. Please load a file first.")
            return
        
        prompt = f"""Generate comprehensive documentation for the following {self.language} code.
        Include:
        1. Overview of what the code does
        2. Function/method documentation with parameters and return values
        3. Usage examples if applicable
        4. Any dependencies or requirements
        
        Format your response in Markdown with appropriate headings.
        
        Code:
        {self.file_content}
        """
        
        try:
            print("Generating documentation...")
            response = self.model.generate_content(prompt)
            
            # Handle output based on preference
            output_content = f"# Documentation for {self.filename}\n\n{response.text}"
            self.handle_output(output_content, f"{self.filename}_documentation.md")
            
            return response.text
        except Exception as e:
            print(f"Error generating documentation: {e}")
            return None
    
    def quick_summary(self):
        """Generate a quick summary of the code without detailed analysis"""
        if not self.file_content:
            print("No file loaded. Please load a file first.")
            return
        
        prompt = f"""Provide a concise summary of the following {self.language} code.
        Focus on the main purpose and key functionality in 3-5 sentences.
        
        Code:
        {self.file_content[:3000]}  # Limit length for a quick response
        """
        
        try:
            print("Generating quick summary...")
            response = self.model.generate_content(prompt)
            
            # For quick summary, always print to console
            print("\n" + "="*60)
            print("QUICK SUMMARY")
            print("="*60)
            print(response.text)
            print("="*60)
            
            return response.text
        except Exception as e:
            print(f"Error generating quick summary: {e}")
            return None
    
    def display_menu(self):
        """Display the main menu"""
        print("\n" + "="*50)
        print("           THE CODE RIFFACTOR")
        print("="*50)
        print("1. Identify programming language")
        print("2. Perform code review")
        print("3. Create beginner-friendly analysis")
        print("4. Refactor for different paradigms")
        print("5. Suggest improvements")
        print("6. Generate documentation")
        print("7. Quick summary (console only)")
        print("8. Set output preference (current: " + self.output_preference + ")")
        print("9. Load a different file")
        print("0. Exit")
        print("="*50)
    
    def run(self):
        """Main program loop"""
        print("Welcome to The Code Riffactor!")
        
        # Setup Gemini API
        if not self.setup_gemini():
            print("Failed to initialize Gemini API. Exiting.")
            return
        
        # Load a file
        if not self.load_file():
            print("Failed to load file. Exiting.")
            return
        
        while True:
            self.display_menu()
            choice = input("Enter your choice (0-9): ").strip()
            
            if choice == "1":
                self.identify_language()
            elif choice == "2":
                self.code_review()
            elif choice == "3":
                self.beginner_analysis()
            elif choice == "4":
                self.refactor_for_paradigms()
            elif choice == "5":
                self.suggest_improvements()
            elif choice == "6":
                self.generate_documentation()
            elif choice == "7":
                self.quick_summary()
            elif choice == "8":
                self.set_output_preference()
            elif choice == "9":
                if self.load_file():
                    self.language = None  # Reset language when loading new file
            elif choice == "0":
                print("Thank you for using The Code Riffactor!")
                break
            else:
                print("Invalid choice. Please try again.")
            
            input("\nPress Enter to continue...")

# Run the program
if __name__ == "__main__":
    riffactor = CodeRiffactor()
    riffactor.run()