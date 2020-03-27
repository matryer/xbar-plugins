#!/bin/bash
# <bitbar.title>DiskAvailable</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kalak Lanar</bitbar.author>
# <bitbar.author.github>kalaklanar</bitbar.author.github>
# <bitbar.desc>displays free disk space of the root volume</bitbar.desc>
# <bitbar.</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

/usr/sbin/diskutil info /dev/disk1s1 |/usr/bin/awk '$0 ~ /Volume Free Space/ {print $4$5}'
