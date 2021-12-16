#!/bin/bash

# <xbar.title>Viscosity active connection</xbar.title>
# <xbar.desc>Shows the name of the first active connection</xbar.desc>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Adam Bogdał</xbar.author>
# <xbar.author.github>bogdal</xbar.author.github>
# <xbar.dependencies>bash,viscosity</xbar.dependencies>

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
