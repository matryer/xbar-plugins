#!/bin/bash

# <bitbar.title>Persian Date</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Ilia Vakili</bitbar.author>
# <bitbar.author.github>theReticent</bitbar.author.github>
# <bitbar.desc>Shows Persian date</bitbar.desc>
# <bitbar.image></bitbar.image>
# <bitbar.dependencies>jcal</bitbar.dependencies>

# To fix the "command not found" caused by installing jcal using brew
PATH=/usr/local/bin:$PATH

jdate "+%W"
echo "---"
jdate "+%G %d %V %Y"
