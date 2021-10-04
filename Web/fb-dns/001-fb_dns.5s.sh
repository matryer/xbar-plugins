#!/usr/bin/env bash

# <xbar.title>FB DNS</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Spencer Brown, WebSpence</xbar.author>
# <xbar.author.github>spenweb</xbar.author.github>
# <xbar.desc>Monitors Facebook DNS and tracks downtime</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/spenweb/fb-dns/main/screen-shot.png</xbar.image>
# <xbar.dependencies>bash,bind</xbar.dependencies>
# <xbar.abouturl>https://github.com/spenweb/fb-dns</xbar.abouturl>

# ==============================CONFIGURATION================================
# Set these variables to configure the output to your liking.
RECORDS=a
DOMAIN=facebook.com
UNIX_FB_DOWN_START=1633360740
# ===========================================================================



# ==============================DEPENDENCIES=================================
# This script requires bind using the dig tool for querying DNS.
# Requires: https://www.isc.org/bind/
# Install via brew:  `brew install bind`
# ===========================================================================

# ===============================DATA SOURCE=================================
# DNS
# https://downdetector.com/status/facebook/
# ===========================================================================



# Setting my Bitbar path to include /usr/local/bin. Systems may vary
# jq fails without this
LANG=en_US.UTF-8 # needed in BitBar env for awk to format numbers
export LANG

function get_elapsed() {
  # Rough start time of Facebook being downon Downdetector.com
  start=$UNIX_FB_DOWN_START
  now=$(date -u +%s)
  diff=$(( (now - start) / 60 ))
  echo "Down $diff minutes | color=#b01000"
}

RESPONSE=`dig $RECORDS $DOMAIN +short`
if [[ -z $RESPONSE ]]; then
  echo "FB down! | color=#b01000"
  echo "---"
  get_elapsed
else
  echo "FB back up!"
  echo "---"
  echo "Answer section:"
  echo $RESPONSE
fi

echo "Downdetector | href=https://downdetector.com/status/facebook/"
echo "#DNS | color=#0048b0 href=https://twitter.com/hashtag/DNS"

