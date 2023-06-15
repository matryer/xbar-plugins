#!/usr/bin/env bash
# <xbar.title>Internet Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Afraz Ahmadzadeh</xbar.author>
# <xbar.author.github>afrazkhan</xbar.author.github>
# <xbar.desc>Checks network and DNS functionality</xbar.desc>

ping_timeout=1
ping_address=9.9.9.9
dns_address=www.quad9.net

# By default, everything is fine
icon="🌎|dropdown=false"
message="All is well"
dns_icon="✅"
ping_icon="✅"

ping -c 1 -t $ping_timeout -q $ping_address > /dev/null 2>&1
ping_result=$?
host $dns_address > /dev/null
dns_result=$?

if ! [ $ping_result == 0 ]; then
  icon="🌐|color=#f23400 dropdown=false"
  message="Ping failed"
  ping_icon="❌"
fi

if ! [ $dns_result == 0 ]; then
  icon="🕸️|color=#f23400 dropdown=false"
  message="DNS failed"
  dns_icon="❌"
fi

if ! [ $ping_result == 0 ] && ! [ $dns_result == 0 ]; then
  icon="☠️|color=#f23400 dropdown=false"
  message="DNS and ping failed"
  dns_icon="❌"
  ping_icon="❌"
fi

echo "$icon"
echo "---"
echo "PING: $ping_icon"
echo "DNS: $dns_icon"
echo "---"
echo "$message"
