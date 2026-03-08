#!/usr/bin/env python3

# <xbar.title>Deep Work Guardian</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Gemini</xbar.author>
# <xbar.author.github>google</xbar.author.github>
# <xbar.desc>Tracks active window to ensure you are focusing on work apps.</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>

import os
import sys
import json
import time
import subprocess

# --- CONFIGURATION ---

# Apps that count as "Work" (Green status)
WORK_APPS = ["Code", "Terminal", "iTerm2", "Obsidian", "Figma", "Slack", "Island", "Xcode"]

# Apps that count as "Distractions" (Red status)
DISTRACTION_APPS = ["Messages", "Discord", "Mail", "Safari", "Chrome"]

# File to store session state
STATE_FILE = "/tmp/xbar_focus_state.json"

# --- LOGIC ---

def get_active_app():
    """Uses AppleScript to get the name of the frontmost application."""
    script = 'tell application "System Events" to get name of first application process whose frontmost is true'
    try:
        result = subprocess.check_output(['osascript', '-e', script])
        return result.decode('utf-8').strip()
    except Exception:
        return None

def load_state():
    if os.path.exists(STATE_FILE):
        try:
            with open(STATE_FILE, 'r') as f:
                return json.load(f)
        except:
            return {"active": False, "start_time": 0}
    return {"active": False, "start_time": 0}

def save_state(state):
    with open(STATE_FILE, 'w') as f:
        json.dump(state, f)

def format_time(seconds):
    mins, secs = divmod(int(seconds), 60)
    return f"{mins}m"

def handle_actions():
    """Handles clicks from the menu bar."""
    if len(sys.argv) > 1:
        action = sys.argv[1]
        state = load_state()
        
        if action == "start":
            state["active"] = True
            state["start_time"] = time.time()
        elif action == "stop":
            state["active"] = False
        
        save_state(state)
        # Force xbar to refresh immediately (optional, but snappy)
        # subprocess.run(["open", "-g", "xbar://app.xbarapp.com/refresh"]) 
        sys.exit()

# --- MAIN RUN ---

if __name__ == "__main__":
    handle_actions() # Check if user clicked a button
    
    state = load_state()
    current_app = get_active_app()
    
    # Header Font handling
    font_str = "| font=Menlo size=12"
    
    if not state["active"]:
        # IDLE STATE
        print(f"Waiting... {font_str}")
        print("---")
        print("Start Deep Work Session | bash='" + sys.argv[0] + "' param1=start terminal=false refresh=true")
        print("---")
        print(f"Current App: {current_app}")
    
    else:
        # ACTIVE FOCUS STATE
        duration = time.time() - state["start_time"]
        time_str = format_time(duration)
        
        if current_app in DISTRACTION_APPS:
            # RED ALERT
            print(f"⚠️ GET BACK TO WORK ({time_str}) | color=red {font_str}")
            print("---")
            print(f"Distraction detected: {current_app} | color=red")
        
        elif current_app in WORK_APPS:
            # IN THE ZONE
            print(f"🧠 Deep Work: {time_str} | color=green {font_str}")
            print("---")
            print(f"Good job using: {current_app} | color=green")
            
        else:
            # NEUTRAL APP
            print(f"Deep Work: {time_str} | color=orange {font_str}")
            print("---")
            print(f"Current App: {current_app}")

        # Stop Button
        print("---")
        print("Stop Session | bash='" + sys.argv[0] + "' param1=stop terminal=false refresh=true")