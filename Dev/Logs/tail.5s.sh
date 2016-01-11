#!/bin/bash
# <bitbar.title>tail</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Tails a text file, set `FILE` env var. Perfect for tailing logs in the menu bar.</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/101659/12247623/b65b6f1e-b8ac-11e5-8ec2-6d9d885bfb6f.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Dev/Logs/tail.5s.sh</bitbar.abouturl>

LINES=15
FILE="path/to/file/to/tail"

echo -n "↧ "
basename "$FILE"
echo ---
tail -n "$LINES" "$FILE"
