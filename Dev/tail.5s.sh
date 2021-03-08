#!/bin/bash

# ...
# <xbar.var>string(VAR_FILE=""): The file to tail.</xbar.var>
# <xbar.var>number(VAR_LINES=15): The number of lines to show.</xbar.var>

# If you're using xbar, use the app to install this and set the preferences
# in the UI.
# For old BitBar builds, set the values by uncommenting these lines:
# VAR_FILE=/path/to/file
# VAR_LINES=15

echo -n "↧ "
basename "$VAR_FILE"
echo ---
tail -n "$VAR_LINES" "$VAR_FILE"
