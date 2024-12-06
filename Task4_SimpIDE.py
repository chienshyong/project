import tkinter as tk
from tkinter import filedialog, messagebox, simpledialog
import subprocess
import os

def open_file():
    file_path = filedialog.askopenfilename(
        filetypes=[("Simp Files", "*.simp"), ("All Files", "*.*")]
    )
    if file_path:
        with open(file_path, "r") as file:
            text_editor.delete("1.0", tk.END)  # Clear current text
            text_editor.insert(tk.END, file.read())  # Load file content

def save_file():
    file_path = filedialog.asksaveasfilename(
        defaultextension=".txt",
        filetypes=[("Simp Files", "*.simp"), ("All Files", "*.*")],
    )
    if file_path:
        with open(file_path, "w") as file:
            file.write(text_editor.get("1.0", tk.END))  # Save current text

def clear_text():
    text_editor.delete("1.0", tk.END)  # Clear all text

def confirm_exit():
    if messagebox.askyesno("Exit", "Do you really want to exit?"):
        root.destroy()

def log_message(message):
    """Print a message to the log text box."""
    log_box.insert(tk.END, message + "\n")
    log_box.see(tk.END)  # Scroll to the latest message

def compile_code():
    code = text_editor.get("1.0", tk.END).strip()
    if not code:
        log_message("No code to compile.")
        return
    
    argument = simpledialog.askstring("Compile", "Enter argument:")
    if argument is None or argument.strip() == "":
        log_message("Compilation cancelled: No argument provided.")
        return

    # Create a temporary file
    temp_file_path = './simp/temp/temp.simp'  # Get the path to the temporary file
    with open(temp_file_path, "w") as file:
        file.write(text_editor.get("1.0", tk.END))

    # Run compiler
    log_message("Compiling code and running with argument: " + argument)
    # Path to your Haskell binary file
    binary_path = "./simp/dist-newstyle/build/x86_64-linux/ghc-9.6.6/simp-0.1.0.0/x/simp/build/simp/simp"

    if os.name == "nt":  # Windows
        command = ["wsl", binary_path, "-i", temp_file_path, argument]
    else:  # Linux
        command = [binary_path, "-i", temp_file_path, argument]

    # Run the command using subprocess
    try:
        result = subprocess.run(command, capture_output=True, text=True)
        if result.stdout:
            log_message("Output: " + result.stdout)
        if result.stderr:
            log_message("Error: " + result.stderr)
    except FileNotFoundError:
        log_message("Binary file not found. Check the WSL path.")
    except Exception as e:
        log_message(f"An error occurred: {e}")


# Create main application window
root = tk.Tk()
root.title("Simp Interactive Development Environment")
root.geometry("800x600")

# Create text editor widget
text_editor = tk.Text(root, wrap="word", font=("Arial", 12), height=20)
text_editor.pack(expand=False, fill=tk.BOTH, padx=5, pady=5)

# Create log box widget
log_label = tk.Label(root, text="Logs", font=("Arial", 10, "bold"))
log_label.pack(anchor="w", padx=5)
log_box = tk.Text(root, wrap="word", font=("Arial", 10), height=10, bg="#f4f4f4")
log_box.pack(expand=False, fill=tk.BOTH, padx=5, pady=5)
log_box.config(state=tk.NORMAL)  # Enable typing in log box if needed

# Create menu bar
menu_bar = tk.Menu(root)

# File menu
file_menu = tk.Menu(menu_bar, tearoff=0)
file_menu.add_command(label="Open", command=open_file)
file_menu.add_command(label="Save", command=save_file)
file_menu.add_separator()
file_menu.add_command(label="Exit", command=confirm_exit)
menu_bar.add_cascade(label="File", menu=file_menu)

# Edit menu
edit_menu = tk.Menu(menu_bar, tearoff=0)
edit_menu.add_command(label="Clear", command=clear_text)
menu_bar.add_cascade(label="Edit", menu=edit_menu)

# Compile
compile_menu = tk.Menu(menu_bar, tearoff=0)
compile_menu.add_command(label="Compile and Run", command=compile_code)
# compile_menu.add_command(label="Compile and Run with argument", command=compile_code)
menu_bar.add_cascade(label="Compile & Run", menu=compile_menu)

# Set menu bar
root.config(menu=menu_bar)

# Run the application
root.mainloop()