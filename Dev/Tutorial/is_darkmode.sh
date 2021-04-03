#!/bin/bash

# <xbar.title>Is Dark Mode?</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Dave Wikoff</xbar.author>
# <xbar.author.github>derimagia</xbar.author.github>
# <xbar.desc>Example script showing how to let your scripts determine whether OSX is in Dark Mode so you can adjust appearance. Must restart Bitbar after changing setting.</xbar.desc>
# <xbar.dependencies></xbar.dependencies>
# <xbar.image>http://i.imgur.com/2ark3Bq.png</xbar.image>
BitBarDarkMode=${BitBarDarkMode}

if [ "$BitBarDarkMode" ]; then
  # OSX has Dark Mode enabled.
  echo "Dark | color=white"
else
  # OSX does not have Dark Mode
  echo "Light | color=black"
fi
