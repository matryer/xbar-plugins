#!/bin/bash

# <bitbar.title>Finder Git Info</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Miguel Carvalho</bitbar.author>
# <bitbar.author.github>mmcarvalho</bitbar.author.github>
# <bitbar.desc>Displays the active git branch of the focused finder Window in the menu bar. When open, lists all finder windows with active git projects.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/mmcarvalho/git-finder-info/main/gitfinderinfo-screenshot.png</bitbar.image>
# <bitbar.abouturl>https://github.com/mmcarvalho/git-finder-info</bitbar.abouturl>
#
# Dependencies: none.

read -d '' get_windows <<EOL
tell application "Finder"
	set mywindowlist to the name of every window
	# send result as text to bash - avoid strange chars. 
	# @todo: improve it using 'text item delimiters' or move functionality to bash completely. 
	set windowsPaths to ""
	repeat with thePath in mywindowlist
		set windowsPaths to windowsPaths & thePath & "|"
	end repeat
end tell
EOL
windows_list=$(osascript -e "${get_windows}")
if [ -n "$windows_list" ]; then
	IFS='|'
	read -a paths <<< "$windows_list"
    i=0
    for path in "${paths[@]}"
    do
        git_path="${path}/.git"
        if [ -d "$git_path" ]; then
            branch=$(cd "${path}" && git branch --show-current)
            basename=$(basename ${path})
            if [ "$i" -eq "0" ]; then
                echo "𣎴 $branch"
                echo ---
            fi
			echo "📂 $branch ($basename)"
		else
			if [ "$i" -eq "0" ]; then
                echo "∅ no git dir"
                echo ---
            fi
		fi
        i=$(($i + 1))
    done
	echo ---
	echo "△ Open finder windows with active git projects."
else
    echo "∅ no git dir"
fi

