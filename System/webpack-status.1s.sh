#!/bin/bash
#
# <xbar.title>Webpack Status</xbar.title>
# <xbar.version>v1.1.2</xbar.version>
# <xbar.author>Francesco Belladonna</xbar.author>
# <xbar.author.github>Fire-Dragon-DoL</xbar.author.github>
# <xbar.desc>Display the current status of webpack server compile: completed/ongoing/failed.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/Fire-Dragon-DoL/bitbar-webpack-status-plugin/master/bitbar-webpack-status-plugin.jpg</xbar.image>
# <xbar.dependencies>node,webpack</xbar.dependencies>
# <xbar.abouturl>https://github.com/Fire-Dragon-DoL/bitbar-webpack-status-plugin</xbar.abouturl>

color_red="✘ | color=#ff0000 dropdown=false"
color_yellow="◉ | color=#ffa500 dropdown=false"
color_green="✔︎ | color=#008000 dropdown=false"

webpack_status="/tmp/webpack-status"

semaphore=0

if [ -f $webpack_status ]; then
  semaphore=$(cat $webpack_status)
fi

semaphore_color=$color_yellow

# -1 red, 0 yellow, 1 green
case $semaphore in
-1)
  # Red
  semaphore_color=$color_red
  ;;
1)
  # Green
  semaphore_color=$color_green
  ;;
*)
  # Orange
  semaphore_color=$color_yellow
  ;;
esac

echo "$semaphore_color"
echo "---"
echo "Clear Webpack Status | terminal=false bash=rm param1=-f param2='$webpack_status'"
