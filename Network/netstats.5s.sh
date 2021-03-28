#!/bin/bash

# <xbar.title>IP Address Info</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jack Higgins</xbar.author>
# <xbar.author.github>skhg</xbar.author.github>
# <xbar.desc>Displays your local IP address with useful extra info</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/skhg/BitBar-Plugins/master/NetworkInfo/ip_info.jpg</xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl>https://github.com/skhg/BitBar-Plugins/tree/master/NetworkInfo</xbar.abouturl>
# <xbar.var>number(VAR_WARNING_SPEED=20): When the connection to the router drops below this speed (Mbps) your IP address will be highlighted in orange</xbar.var>
# <xbar.var>string(VAR_NETWORK_INTERFACE="en0"): The interface to track (Usual interface for MacOS wifi is en0)</xbar.var>


if [[ -z "${VAR_NETWORK_INTERFACE}" ]]
then
    # VAR_NETWORK_INTERFACE is not set. Get it with help from @hoondi (https://github.com/matryer/xbar-plugins/issues/1512)
    VAR_NETWORK_INTERFACE=$(route -n get default | grep -o "interface: .*" | sed -e 's/interface: //')
fi


LOCAL_IP=$(ipconfig getifaddr ${VAR_NETWORK_INTERFACE} 2>&1)
LOCAL_OK=$?

if [[ $LOCAL_OK != 0 ]] ; then
    LOCAL_PART="❌"
    ROUTER_PART="❌ - Router"
else
    LOCAL_PART=$LOCAL_IP

    ROUTER=$(netstat -nr | grep default | grep -E -o '\d+\.\d+\.\d+\.\d+' 2>&1)
    ROUTER_OK=$?

    if [[ $ROUTER_OK != 0 ]] ; then
        ROUTER_PART="Unable to determine router IP? | color=orange"
    else
        ROUTER_PART="$ROUTER"" - Router"
    fi
fi

REMOTE_IP=$(dig +short myip.opendns.com @resolver1.opendns.com 2>&1)
# Alternatively, you can use:
# REMOTE_IP=$(curl ifconfig.me 2>&1)

REMOTE_OK=$?

if [[ $REMOTE_OK != 0 ]] ; then
    REMOTE_PART="❌"
else
    REMOTE_PART="$REMOTE_IP"
fi

SPEED=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | grep 'lastTxRate:' | grep -o '\d\+' 2>&1)

if [[ $LOCAL_OK != 0 ]] ; then
    SPEED_PART="❌"
    SPEED_WARNING=0
else
    SPEED_PART="$SPEED""Mbps"

    if [[ "$SPEED" -lt ${VAR_WARNING_SPEED} ]] ; then
        SPEED_WARNING=1
    else
        SPEED_WARNING=0
    fi
fi



if [[ $REMOTE_OK != 0 ]] ; then
    REMOTE_WARNING=1
else
    REMOTE_WARNING=0
fi

function speedcolour {
    SPEED=$1

    if [[ "$SPEED" == 1 ]] ; then
        echo " color=orange"
        return
    fi

    echo ""
}

function wancolour {
    WAN=$1

    if [[ "$WAN" == 1 ]] ; then
        echo " color=red"
        return
    fi

    echo ""
}

function topcolour {
    SPEED=$1
    WAN=$2

    if [[ "$WAN" == 1 ]] ; then
        wancolour "$WAN"
        return
    fi

    if [[ "$SPEED" == 1 ]] ; then
        speedcolour "$SPEED"
        return
    fi

    echo ""
}

echo "$LOCAL_PART | $(topcolour $SPEED_WARNING $REMOTE_WARNING) font=Courier"

echo "---"

echo "$LOCAL_PART - Local | font=Courier"
echo "$ROUTER_PART | font=Courier"
echo "$SPEED_PART - LAN Speed | $(speedcolour $SPEED_WARNING) font=Courier"
echo "$REMOTE_PART - WAN | $(wancolour $REMOTE_WARNING) font=Courier"

echo "---"

echo "Terminal: ifconfig| bash='ifconfig'"
echo "Terminal: Adapter Info| bash='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I'"
echo "Terminal: Wireless Scan| bash='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -s'"

echo "---"

echo "Router Web Config | href=http://$ROUTER_PART"
