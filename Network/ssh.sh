#!/bin/zsh
#
# Quickly SSH to your favorite hosts
# The list of hosts are extracted from ~/.ssh/config
#
# <xbar.title>SSH</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Thameera Senanayaka</xbar.author>
# <xbar.author.github>thameera</xbar.author.github>
# <xbar.desc>Quickly SSH to your favorite hosts listed in your ~/.ssh/config file</xbar.desc>
#

echo "ssh"
echo "---"
awk '/^Host / && !/\*/ {print $2" | shell=ssh param1="$2 " terminal=true"}' ~/.ssh/config
