#!/bin/bash

# <bitbar.title>Timezone</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Toni Hoffmann</bitbar.author>
# <bitbar.author.github>xremix</bitbar.author.github>
# <bitbar.desc>Show the current time of a different timezone.</bitbar.desc>

Prefix="FL"
Time_Zone="US/Eastern"
TZ=":$Time_Zone" date "+$Prefix %H:%M"
