#!/bin/bash

# Get current status of a VPN connection with options to connect/disconnect.
# Working with OpenConnect, but can work with any executable VPN. Commands
# that require admin permissions should be whitelisted with 'visudo', e.g.:
#
#joesmith ALL=(ALL) NOPASSWD: /usr/local/bin/openconnect
#joesmith ALL=(ALL) NOPASSWD: /usr/bin/killall -2 openconnect

# <xbar.title>VPN Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jesse Jarzynka</xbar.author>
# <xbar.author.github>jessejoe</xbar.author.github>
# <xbar.desc>Displays status of a VPN interface with option to connect/disconnect.</xbar.desc>
# <xbar.image>http://i.imgur.com/RkmptwO.png</xbar.image>

VPN_EXECUTABLE=/usr/local/bin/openconnect
VPN_EXECUTABLE_PARAMS="--script=$HOME/scripts/vpnc-script-no-dns" # Optional
VPN_HOST="vpn.mydomain.com"
VPN_INTERFACE="utun0"
VPN_USERNAME="joe.smith"
# A command that will result in your VPN password. Recommend using
# "security find-generic-password -g -a foo" where foo is an account
# in your OSX Keychain, to avoid passwords stored in plain text
GET_VPN_PASSWORD="security find-generic-password -g -a joe.smith 2>&1 >/dev/null | cut -d'\"' -f2"
#GET_VPN_PASSWORD="cat ~/.vpnpass"
#GET_VPN_PASSWORD="echo hunter2" # Not recommended
# Command to determine if VPN is connected or disconnected
VPN_CONNECTED="ifconfig | egrep -A1 $VPN_INTERFACE | grep inet"
# Command to run to disconnect VPN
VPN_DISCONNECT_CMD="sudo killall -2 openconnect"

case "$1" in
    connect)
        VPN_PASSWORD=$(eval "$GET_VPN_PASSWORD")
        # VPN connection command, should eventually result in $VPN_CONNECTED,
        # may need to be modified for VPN clients other than openconnect
        echo "$VPN_PASSWORD" | sudo "$VPN_EXECUTABLE" "$VPN_EXECUTABLE_PARAMS" --user "$VPN_USERNAME" --passwd-on-stdin "$VPN_HOST" &> /dev/null &
        # Wait for connection so menu item refreshes instantly
        until eval "$VPN_CONNECTED"; do sleep 1; done
        ;;
    disconnect)
        eval "$VPN_DISCONNECT_CMD"
        # Wait for disconnection so menu item refreshes instantly
        until [ -z "$(eval "$VPN_CONNECTED")" ]; do sleep 1; done
        ;;
esac

if [ -n "$(eval "$VPN_CONNECTED")" ]; then
    echo "VPN ✔"
    echo '---'
    echo "Disconnect VPN | bash='$0' param1=disconnect terminal=false refresh=true"
    exit
else
    echo "VPN ✘"
    echo '---'
    echo "Connect VPN | bash='$0' param1=connect terminal=false refresh=true"
    exit
fi
