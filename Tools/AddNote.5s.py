#!/usr/bin/env python
# -*- encoding: utf-8 -*-

# <bitbar.title>Add Note</bitbar.title>
# <bitbar.author>Frak Nuaimy</bitbar.author>
# <bitbar.author.github>frakman1</bitbar.author.github>
# <bitbar.image>http://i62.tinypic.com/1zvae6o.jpg</bitbar.image>
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
        cmd = "osascript -e \'set theString to text returned of (display dialog \"Please Enter The Note To Add to \" with icon note default answer \"\n\n\n\" buttons {\"OK\",\"Cancel\"} default button 1) \'" 
        note = run_script(cmd)
        print len(note)
        if len(note) is 0:
             sys.exit(1)
        if "\n" in note:
            i = note.index('\n')
            header = note.splitlines()[0]
            body = note[i+1:]
        else:
            header = note
            body = ""
        print note
        cmd2 = "osascript -e 'tell application \"Notes\" \n tell account \"iCloud\" \n make new note at folder \"Notes\" with properties {name:\"%s\", body:\"%s\"} \n end tell \n end tell'" % (header, body)
        run_script2(cmd2)
        #print note
        sys.exit(1)

print "📔"
print "---"
print("AddNote | trim=false, color=yellow bash=" + fullPathFileName +  " param1=-n param2=null terminal=false refresh=true")

