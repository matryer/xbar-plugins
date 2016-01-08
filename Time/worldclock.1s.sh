#!/bin/bash

# Display UTC in the menubar, and one or more additional zones in the drop down.
# The current format (HH:MM:SS) works best with a one second refresh, or alter
# the format and refresh rate to taste.
#
# Adam Snodgrass <asnodgrass@sarchasm.us>

ZONES="Australia/Sydney Europe/Amsterdam America/New_York America/Los_Angeles"
date -u +'%H:%M:%S UTC'
echo '---'
for zone in $ZONES; do
  echo "$(TZ=$zone date +'%H:%M:%S %z') $zone"
done
