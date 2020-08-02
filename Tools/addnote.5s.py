#!/usr/bin/env python
# -*- encoding: utf-8 -*-

# <bitbar.title>Add Note</bitbar.title>
# <bitbar.author>Frak Nuaimy</bitbar.author>
# <bitbar.author.github>frakman1</bitbar.author.github>
# <bitbar.image>http://i.imgur.com/608LQ25.png</bitbar.image>
# <bitbar.desc>Add a note to Apple Notes app.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.version>v1.0</bitbar.version>


import os, sys
import argparse
import subprocess


def run_script(script):
    return (subprocess.Popen([script], stdout=subprocess.PIPE, shell=True).communicate()[0].strip()).replace("'", "’")

def run_script2(script):
    return (subprocess.Popen([script], stdout=subprocess.PIPE, shell=True).communicate()[0])

fullPathFileName = os.path.realpath(__file__)


parser = argparse.ArgumentParser()
parser.add_argument('-n', action='store', dest='localnote',help='Create Note Flag')
results = parser.parse_args()


if(len(sys.argv) >= 2):
    if (sys.argv[1] == "-n"): 
        cmd = "osascript -e \'set theString to text returned of (display dialog \"Please Enter The Note To Add \" with icon note default answer \"\n\n\n\" buttons {\"OK\",\"Cancel\"} default button 1) \'" 
        note = run_script(cmd)
        if len(note) is 0:
             sys.exit(1)
        if "\n" in note:
            i = note.index('\n')
            header = note.splitlines(True)[0]
            body = note[i+1:]
        else:
            header = note
            body = ""
        cmd2 = "osascript -e 'tell application \"Notes\" \n tell account \"iCloud\" \n make new note at folder \"Notes\" with properties {name:\"%s\", body:\"%s\"} \n end tell \n end tell'" % (header, body)
        run_script2(cmd2)
        sys.exit(1)
note_icon = "iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAACXBIWXMAAA7DAAAOwwHHb6hkAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAf9JREFUOI21lE1rE1EYhc97c9PG2C+VIp0mLpqg0I0EV67FneBvqEL/hQGrC8GNiiJFXGRR/4Eg1PqBbqxiFwVta03BTKdJtHWSaeajM3NfFyUlhgqJmR54V+fycO/hvJcA4NHtqTRArypGbcKy9gQzoxsNDPTx6NhwdaVYuv648PI5ANDsnekzv8rW+rfVYlwp1RWwVUSE9ERabdXMa0/nFgrCte35tZXeoADAzCgVS+KcNvoAwMnY+UzqvlVvUE/UFjlO0N+XFG+F+dsRUUH3wS4ms2MX5JWrZxH4XmRgohg+fSmTvHQ5C7AfGRggbGxtxwTQXbU6kVJKRJrvgYSAdG0f4DBCKkGFCjJxTAKIrG37YJA4gowZIYd8JBlzGJJ0bB8U4a2ZAQ4pJhNJGTk4CAKI8s+gRvFTaB+j3OiBrkhkcjc0w6jU2z3TtPFDN/+Lq5iFBGBncvnU96UZXdNODzVNP1AwjDo8J0B6fKRjqO8rLH7W9WYrrEwun9o0KlbzQFwK9Cdi2HU8VHd2kUjKjsYJAu/F69WPrXWzsrm81gpvanun0VEsZs3F3XsLcwC+HrZyQ+tLM7qhVwdd7+/v9MTIcaTHhw95fgiz7u49fPLu2Wzhwy0AxX/t8uD01MWb7HO83YhLEQKAIFIAQES8vLZpvHm/sQhgGYAJAH8As2PnfRzL6c4AAAAASUVORK5CYII="
print " | image={}".format(note_icon) + " trim=false, color=yellow bash=" + fullPathFileName +  " param1=-n param2=null terminal=false refresh=true"

