#!/bin/bash

# <xbar.title>Persian Date</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Ilia Vakili</xbar.author>
# <xbar.author.github>theReticent</xbar.author.github>
# <xbar.desc>Shows Persian date</xbar.desc>
# <xbar.image></xbar.image>
# <xbar.dependencies>jcal</xbar.dependencies>

# To fix the "command not found" caused by installing jcal using brew
PATH=/usr/local/bin:$PATH

jdate "+%W"
echo "---"
jdate "+%G %d %V %Y"
