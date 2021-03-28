#!/bin/bash

# <xbar.title>Caffeinate</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Steffen Froehlich</xbar.author>
# <xbar.author.github>citoki</xbar.author.github>
# <xbar.desc>This plugin will give some caffeine, with lots of sugar, to your Mac to stay awake.
# Technically the commandline tool 'caffeinate' is executed.</xbar.desc>
# <xbar.image>https://i.imgur.com/vsCWLwX.png</birtbar.image>
# <xbar.dependencies></xbar.dependencies>

if [ "$1" = 'sugar' ]; then
  # stop all previous processes
  /usr/bin/killall caffeinate

  # start caffeinate program and prevent the system from idle sleeping when
  # Mac OSX is running on AC power
  /usr/bin/caffeinate -s
fi

if [ "$1" = 'caffeine' ]; then
  # stop all previous processes
  /usr/bin/killall caffeinate

  # start caffeinate program and prevent the system from idle sleeping
  /usr/bin/caffeinate -i
fi

if [ "$1" = 'stop' ]; then
  # send terminate signal to all processes of program 'caffeinate'
  /usr/bin/killall caffeinate
fi

echo "☕️"
echo '---'
echo "Sugar - stay awake w/ AC power| bash='$0' param1=sugar terminal=false"
echo "Caffeine - stay awake w/o AC power | bash='$0' param1=caffeine terminal=false"
echo "Stop caffeine dose | bash='$0' param1=stop terminal=false"

