#!/bin/bash
# WiZ lights
#
#  <xbar.title>WiZ light</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Agustin B.</xbar.author>
#  <xbar.author.github>redraw</xbar.author.github>
#  <xbar.desc>Control a WiZ light</xbar.desc>
#  <xbar.image>https://i.imgur.com/2C77SMl.png</xbar.image>
#  <xbar.dependencies>jq</xbar.dependencies>

# Variables become preferences in the app:
#
#  <xbar.var>string(VAR_IP="192.168.0.176"): WiZ light IP address.</xbar.var>
#  <xbar.var>number(VAR_PORT=38899): WiZ light port number.</xbar.var>

export PATH="/usr/local/bin:$PATH"
IP=${VAR_IP:-192.168.0.176}
PORT=${VAR_PORT:-38899}

action=$1
state=$(echo '{"method": "getPilot"}' | nc -u $IP $PORT -w1 | jq ".result")

echo ":bulb:"
echo "---"
echo "on | shell='$0' param1=on | refresh=true"
echo "off | shell='$0' param1=off | refresh=true"
echo "night mode | shell='$0' param1=night | refresh=true"
echo "---"
echo "brighter | shell='$0' param1=brighter | key=CmdOrCtrl+shift+k | refresh=true"
echo "darker | shell='$0' param1=darker | key=CmdOrCtrl+shift+j | refresh=true"
echo "warmer | shell='$0' param1=warmer | key=CmdOrCtrl+shift+h | refresh=true"
echo "colder | shell='$0' param1=colder | key=CmdOrCtrl+shift+l | refresh=true"
echo "---"

echo "IP: $IP | disabled=true | size=10"
# echo "state: $state"

send() {
    nc -u $IP $PORT -w1 > /dev/null
}

case $action in
    "on")
        echo '{"method": "setPilot", "params": {"state": true}}' | send
        ;;
    "off")
        echo '{"method": "setPilot", "params": {"state": false}}' | send
        ;;
    "night")
        echo '{"method": "setPilot", "params": {"sceneId": 14}}' | send
        ;;
    "brighter")
        dimming=$(echo $state | jq -r .dimming)
        echo '{"method": "setPilot", "params": {"dimming": '$((dimming + 10))'}}' | send
        ;;
    "darker")
        dimming=$(echo $state | jq -r .dimming)
        echo '{"method": "setPilot", "params": {"dimming": '$((dimming - 10))'}}' | send
        ;;
    "warmer")
        temp=$(echo $state | jq -r .temp)
        echo '{"method": "setPilot", "params": {"temp": '$((temp - 500))'}}' | send
        ;;
    "colder")
        temp=$(echo $state | jq -r .temp)
        echo '{"method": "setPilot", "params": {"temp": '$((temp + 500))'}}' | send
        ;;
esac

