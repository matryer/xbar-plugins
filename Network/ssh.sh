#!/usr/bin/env bash
#
# Quickly SSH to your favorite hosts
# The list of hosts are extracted from ~/.ssh/config
#
# <bitbar.title>SSH</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Thameera Senanayaka</bitbar.author>
# <bitbar.author.github>thameera</bitbar.author.github>
# <bitbar.desc>Quickly SSH to your favorite hosts listed in your ~/.ssh/config file</bitbar.desc>
#

echo "ssh"
echo "---"
awk  '/^Host / && !/\*/ {print $2" | bash=ssh param1="$2}' ~/.ssh/config
