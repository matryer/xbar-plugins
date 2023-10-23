#!/usr/bin/env bash
# <xbar.title>Internet Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Afraz Ahmadzadeh</xbar.author>
# <xbar.author.github>afrazkhan</xbar.author.github>
# <xbar.desc>Checks network and DNS functionality</xbar.desc>
# <xbar.image>https://github-production-user-asset-6210df.s3.amazonaws.com/5545555/259845977-79437af9-a1e0-401a-9c34-0e1c6126543f.png</xbar.image>
# <xbar.var>number(VAR_PING_TIMEOUT=1): Ping timeout in seceonds</xbar.var>
# <xbar.var>string(VAR_PING_ADDRESS=9.9.9.9): IP address to ping</xbar.var>
# <xbar.var>string(VAR_DNS_ADDRESS=www.quad9.net): DNS address to query</xbar.var>

ping_timeout=$VAR_PING_TIMEOUT
ping_address=$VAR_PING_ADDRESS
dns_address=$VAR_DNS_ADDRESS

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
