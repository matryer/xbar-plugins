#!/bin/bash

# <bitbar.title>AKAlias</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Isaac</bitbar.author>
# <bitbar.author.github>irstacks</bitbar.author.github>
# <bitbar.desc>View and manage short term bash aliases.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/67rqwnC.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

# MISSION STATEMENT
#
# Creates a lookup for useful and timely commands (ie current-project-specific)
# Can save a little typing and a lot of remembering.
#
# - Prints each line your aka file.
# - Copies the command to your clipboard when clicked on.
# - Provides a link to edit the file.


# SET UP.
#
# Create a source aka.sh file of your own.
# Make sure it's being sourced by .bash_profile, .bashrc,
# or your own fancy .dotfiles config.
#
# Then set the path in VARS below. That's it.

# VARS
#
# Set your own aka.sh/.zsh/.whatever path here.
aka_file=~/.dotfiles/bashers/aka.zsh


# FUNCS
#
# Click to open aka file in terminal with system's visual editor or vi as fallback.
if [[ "$1" = "editaka" ]]; then
	# use system EDITOR or VISUAL or fallback default vi
	# http://stackoverflow.com/questions/10725238/opening-default-text-editor-in-bash
	"${EDITOR:-${VISUAL:-vi}}" "$aka_file"
fi
#
# If user clicked on a history item, copy it back to the clipboard
# Thank you, Jason Tokoph (jason@tokoph.net), via Clipboard history
if [[ "$1" = "copy" ]]; then
  echo "$2" | pbcopy
  exit
fi


# OUTPUTS
#
# Header.
echo "= | color=#1853C9"  # ∷ ⚯ ɐ ª
echo '---'

# Read each line in aka file.
while read -r line
do
	# Strip 'alias ' from line for legibility.
	dict="${line//alias /}"
	# Find everything between " "
	 #shellcheck disable=SC2116,SC2086
	com="$(echo "$line" | grep -o '".*"' | sed 's/"//g')"

	# Echo dict with com as single-quoted p2 argument.
	# Clicking will copy just the command with pbcopy.
	# echo "$(echo $dict) |bash='$0' param1=copy param2='$com' refresh=false terminal=false"
    #shellcheck disable=SC2116,SC2086
	echo "$(echo $dict) |bash='$0' param1=copy param2='$com' refresh=false terminal=false"

done < "$aka_file"

# OPTION TO EDIT AKA FILE
echo ''
echo "---"
echo "Edit .aka file |bash='$0' param1=editaka refresh=true terminal=true"

