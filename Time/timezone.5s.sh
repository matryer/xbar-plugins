#!/bin/bash

# <xbar.title>Timezone</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Toni Hoffmann</xbar.author>
# <xbar.author.github>xremix</xbar.author.github>
# <xbar.desc>Show the current time of a different timezone.</xbar.desc>

Prefix="FL"
Time_Zone="US/Eastern"
TZ=":$Time_Zone" date "+$Prefix %H:%M"
