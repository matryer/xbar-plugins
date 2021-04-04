#!/usr/bin/env bash

# <xbar.title>CPU Load</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Paul W. Rankin</xbar.author>
# <xbar.author.github>rnkn</xbar.author.github>
# <xbar.desc>Shows CPU load as a percentage (without using top).</xbar.desc>
# <xbar.image>https://i.imgur.com/B6VAsDg.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

# BitBar CPU Load plugin

ncpu=$(sysctl -n hw.logicalcpu)
total=$(ps -Ao %cpu= | paste -sd+ - | bc)
usage=$(echo "scale = 2; $total / $ncpu" | bc)

printf "%0.1f%%\n" "$usage"
echo "---"
echo "Refresh | refresh=true"
