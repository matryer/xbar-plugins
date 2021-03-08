#!/bin/bash

# <bitbar.title>tail</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Tails a text file. Perfect for tailing logs in the menu bar.</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/101659/12247623/b65b6f1e-b8ac-11e5-8ec2-6d9d885bfb6f.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Dev/Logs/tail.5s.sh</bitbar.abouturl>

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
tail -n $VAR_LINES $VAR_FILE
