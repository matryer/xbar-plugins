#!/usr/bin/env bash

# <xbar.title>Swap Usage</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Andrzej Wojciechowski (AAWO)</xbar.author>
# <xbar.author.github>AAWO</xbar.author.github>
# <xbar.desc>Swap memory usage indicator</xbar.desc>
# <xbar.image></xbar.image>
# <xbar.dependencies>bash, awk</xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

/usr/sbin/sysctl vm.swapusage | awk '{print "swap " $7}'
