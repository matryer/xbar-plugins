#!/usr/bin/env bash
#
# Quickly SSH to your favorite hosts
# The list of hosts are extracted from ~/.ssh/config
#
# <xbar.title>SSH</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Thameera Senanayaka</xbar.author>
# <xbar.author.github>thameera</xbar.author.github>
# <xbar.desc>Quickly SSH to your favorite hosts listed in your ~/.ssh/config file</xbar.desc>
#

echo "ssh"
echo "---"
awk  '/^Host / && !/\*/ {print $2" | bash=ssh param1="$2}' ~/.ssh/config
