#!/bin/bash

# <bitbar.title>Viscosity active connection</bitbar.title>
# <bitbar.desc>Shows the name of the first active connection</bitbar.desc>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Adam Bogdał</bitbar.author>
# <bitbar.author.github>bogdal</bitbar.author.github>
# <bitbar.dependencies>bash,viscosity</bitbar.dependencies>

CONNECTION_NAME=$(osascript -e '
tell application "Viscosity"
    repeat with _connection in connections
        if the state of _connection is "Connected" then
            return name of _connection
        end if
    end repeat
end tell
')

if [ -n "$CONNECTION_NAME" ]; then
    echo "$CONNECTION_NAME | color=green"
else
    echo "NoVPN | color=#b4b4b4"
fi
