#!/bin/bash

# BitBar timezone plugin
#
# By Toni Hoffmann
#
# Show the current time of a different timezone



Prefix="FL"
Time_Zone="US/Eastern"
TZ=":$TIME_ZONE" date "+$Prefix %H:%M"