#!/bin/bash
# <xbar.title>DiskAvailable</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Kalak Lanar</xbar.author>
# <xbar.author.github>kalaklanar</xbar.author.github>
# <xbar.desc>displays free disk space of the root volume</xbar.desc>
# <xbar.</xbar.image>
# <xbar.dependencies></xbar.dependencies>

#diskutil will report the same as in the Finder (purgable space counted as free)
#df will report what is actually on the disk

DEVICE=$(/bin/df -h |/usr/bin/awk '$9 ~ /^\/$/{print $1}')
diskutil info "$DEVICE" |/usr/bin/awk '$0 ~ /(Container|Volume) Free Space/ {print $4$5}'

#/bin/df -h / | awk '{print $4}' |grep -v 'Avail'
