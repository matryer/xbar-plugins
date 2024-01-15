#!/usr/bin/env python3

# <xbar.title>Wordle</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Anup Sam Abraham</xbar.author>
# <xbar.author.github>anupsabraham</xbar.author.github>
# <xbar.desc>Play the game of wordle on xbar</xbar.desc>
# <xbar.image>https://i.ibb.co/PwLDhxg/SCR-20240115-klhx.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

import subprocess
import random
import getopt
import sys
import json
import os
from pathlib import Path


GAME_FILE = Path("/var/tmp/bitbar_wordle.json")
WORDS_FILE = Path("/usr/share/dict/words")

with open(WORDS_FILE, "r") as f:
    ALL_WORDS = [x.strip().upper() for x in f.readlines()]

MAX_WORD_LENGTH = max([len(x) for x in ALL_WORDS])


def get_input_from_os(alert_text):
    command = (
        "osascript -e 'tell application \"System Events\" to tell process \"SystemUIServer\"' "
        "-e 'set frontmost to true' "
        "-e 'end tell' "
        f"-e 'text returned of (display dialog \"{alert_text}\" "
        "default answer \"\" buttons {{\"OK\"}} default button 1 with title \"Wordle\")'"
    )
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
    output = process.communicate()[0]
    return output.strip()


def write_file(data):
    with open(GAME_FILE, "w") as f:
        f.write(json.dumps(data))


data_exists = "Valid"
if os.path.exists(GAME_FILE):
    with open(GAME_FILE, "r") as f:
        game_data_dump = f.read()
    try:
        game_data = json.loads(game_data_dump)
    except ValueError:
        game_data = {}
        write_file(game_data)
        data_exists = "Corrupt"
else:
    game_data = {}
    data_exists = "No"

if len(sys.argv) > 1:
    opts, args = getopt.getopt(sys.argv[1:], "")
    if args[0] == "restart":
        # Clear data because player wants to restart the game
        game_data = {}
    if args[0] == "game":
        if game_data.get("game_complete") or not game_data.get('char_count'):
            game_data = {}
        if not game_data:
            char_count = get_input_from_os("Number of letters:")
            try:
                char_count = int(char_count)
            except ValueError:
                game_data["error"] = "Invalid letter count"
            else:
                if char_count > MAX_WORD_LENGTH or char_count < 1:
                    game_data["error"] = f"Accepted Number of letters is 1 to {MAX_WORD_LENGTH}"
                else:
                    game_data["char_count"] = char_count

                    valid_words = [x for x in ALL_WORDS if len(x) == char_count]

                    random_word = random.choice(valid_words)
                    game_data["random_word"] = random_word
                    game_data["guesses"] = []
        else:
            char_count = game_data["char_count"]
            random_word = list(game_data["random_word"])
            guess = list(get_input_from_os("Enter your guess").decode('utf-8').strip().upper())

            if len(guess) != char_count:
                game_data["error"] = f"Invalid character length (Enter {char_count} characters)"

            elif "".join(guess) not in ALL_WORDS:
                game_data["error"] = "Word not found"

            elif guess == random_word:
                # Guesssed the correct word. Player won
                game_data["error"] = ""
                game_data["guesses"].append(["".join(guess), "O"*char_count])
                game_data["game_complete"] = "You won"
            else:
                game_data["error"] = ""
                clue = ["X"]*char_count
                found_char = []
                # Check if any of the characters are in the right place
                for index, char in enumerate(guess):
                    if char == random_word[index]:
                        clue[index] = "O"
                        found_char.append(index)
                removed_chars = 0

                # Remove the characters that are in the right place
                for x in found_char:
                    random_word.pop(x-removed_chars)  
                    removed_chars += 1

                # Check if each remaining letters are anywhere in the random word
                for index, char in enumerate(guess):
                    if index not in found_char and char in random_word:
                        clue[index] = "?"
                
                game_data["guesses"].append(["".join(guess), "".join(clue)])
                if len(game_data["guesses"]) >= char_count+1:
                    # Player lost if they're out of turns
                    game_data["game_complete"] = "You lost"

    write_file(game_data)


if data_exists != "Valid" or not game_data:
    if data_exists == "Corrupt":
        print("File corrupted. Restart Game")
    elif data_exists == "No" or not game_data:
        print("Start Game")
    print("---")
    print(f"Start by entering the number | terminal=false bash=\"{sys.argv[0]}\" param1=game refresh=true")
else:
    error = game_data.get("error")
    char_count = game_data.get("char_count")
    guesses = game_data.get("guesses", [])
    game_complete = game_data.get("game_complete")
    if error:
        print("Error")
        print("---")
        print(game_data["error"])
        print(f"Try again | terminal=false bash=\"{sys.argv[0]}\" param1=game refresh=true")
    elif char_count:
        if game_complete:
            print(game_complete)
        else:
            print("Continue game")
    if char_count:
        print("---")
        print(f"{len(game_data.get('guesses'))}/{char_count+1} attempts")
        print("---")
        for each_guess in guesses:
            print(f"{each_guess[0]}")
            print(f"{each_guess[1]}")
            print("---")
        if game_complete:
            print(f"Word: {game_data['random_word']}")
            print(f"{game_complete}")
            print("---")
            print(f"Restart Game | terminal=false bash=\"{sys.argv[0]}\" param1=game refresh=true")    
        else:
            print(f"Enter your guess | terminal=false bash=\"{sys.argv[0]}\" param1=game refresh=true")
            print("---")
            print(f"Restart Game | terminal=false bash=\"{sys.argv[0]}\" param1=restart refresh=true")

# Instructions
print("---")
print("### WORDLE INSTRUCTIONS ###")
print("---")
print("1. Setup:")
print("    - Enter the number of letters in the mystery word.")
print("    - You'll have the same number of chances as letters + 1 to guess.")
print("---")
print("2. Guessing:")
print("    - Enter your word guess, matching the length you specified earlier.")
print("    - Ensure your guess is a valid dictionary word.")
print("---")
print("3. Clues:")
print("    - \"O\" means a correct letter in the right place.")
print("    - \"?\" indicates a correct letter in the wrong place.")
print("    - \"X\" signifies an incorrect letter.")
print("---")
print("4. Objective:")
print("    - Crack the mystery word in the given attempts!")
print("    - Good Luck!!!")
