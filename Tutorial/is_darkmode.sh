#!/bin/bash

# Is Dark Mode?
# BitBar plugin
#
# by Dave Wikoff
#
# Example script showing how to let your scripts determine
# whether Mac is in Dark Mode so you can adjust text colors

if [ $BitBarDarkMode ]; then
  # Mac has Dark Mode enabled.
  echo "Dark | color=white"
else
  # Mac does not have Dark Mode
  echo "Light | color=black"
fi
