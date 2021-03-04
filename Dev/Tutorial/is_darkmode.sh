#!/bin/bash

# <bitbar.title>Is Dark Mode?</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Dave Wikoff</bitbar.author>
# <bitbar.author.github>derimagia</bitbar.author.github>
# <bitbar.desc>Example script showing how to let your scripts determine whether OSX is in Dark Mode so you can adjust appearance. Must restart Bitbar after changing setting.</bitbar.desc>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/2ark3Bq.png</bitbar.image>
BitBarDarkMode=${BitBarDarkMode}

if [ "$BitBarDarkMode" ]; then
  # OSX has Dark Mode enabled.
  echo "Dark | color=white"
else
  # OSX does not have Dark Mode
  echo "Light | color=black"
fi
