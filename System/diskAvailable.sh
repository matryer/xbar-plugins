#!/bin/bash
# <bitbar.title>DiskAvailable</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kalak Lanar</bitbar.author>
# <bitbar.author.github>kalaklanar</bitbar.author.github>
# <bitbar.desc>displays free disk space of the root volume</bitbar.desc>
# <bitbar.</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

#diskutil will report the same as in the Finder (purgable space counted as free)
#df will report what is actually on the disk

DEVICE=$(/bin/df -h |/usr/bin/awk '$9 ~ /^\/$/{print $1}')
/usr/sbin/diskutil info "$DEVICE" |/usr/bin/awk '$0 ~ /Volume Free Space/ {print $4$5}'

#/bin/df -h / | awk '{print $4}' |grep -v 'Avail'
