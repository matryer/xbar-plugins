#!/bin/bash

# <xbar.title>Is BitBar?</xbar.title>
# <xbar.author>Mat Ryer and Tyler Bunnell</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>
# <xbar.desc>Example script showing use of $BITBAR environment variable</xbar.desc>
# <xbar.image>https://i.imgur.com/TcZJI06.png</xbar.image>
# <xbar.version>1.0</xbar.version>
#
# Example script showing how to let your scripts determine
# whether they are expected to deliver BitBar output or not.
#
# Put this script in your BitBar plugins folder and notice
# it says "In BitBar", but run it directly in Terminal, and it
# says "In Terminal".
BitBar=${BitBar}

if [ "$BitBar" ]; then
  # this script is being called from within
  # BitBar.
  echo "In BitBar"
else
  # this script is being called from within
  # Terminal.
  echo "In Terminal"
fi
