#!/bin/bash

# <bitbar.title>IP Address Info</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jack Higgins</bitbar.author>
# <bitbar.author.github>skhg</bitbar.author.github>
# <bitbar.desc>Displays your local IP address with useful extra info</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/skhg/BitBar-Plugins/master/NetworkInfo/ip_info.jpg</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>https://github.com/skhg/BitBar-Plugins/tree/master/NetworkInfo</bitbar.abouturl>





# When the connection to the router drops below this speed (Mbps)
# your IP address will be highlighted in orange

WARNING_SPEED=20











# You don't need to change anything below here...

LOCAL_IP=$(ipconfig getifaddr en0 2>&1)
LOCAL_OK=$?

if [ $LOCAL_OK != 0 ] ; then
    LOCAL_PART="❌"
    ROUTER_PART="❌ - Router | font=Courier"
else
    LOCAL_PART=$LOCAL_IP

    ROUTER=$(netstat -nr | grep default | grep -E -o '\d+\.\d+\.\d+\.\d+' 2>&1)
    ROUTER_OK=$?

    if [ $ROUTER_OK != 0 ] ; then
        ROUTER_PART="Unable to determine router IP? | color=orange font=Courier"
    else
        ROUTER_PART="$ROUTER"" - Router | font=Courier"
    fi
fi

REMOTE_IP=$(dig +short myip.opendns.com @resolver1.opendns.com 2>&1)
# Alternatively, you can use:
# REMOTE_IP=$(curl ifconfig.me 2>&1)

REMOTE_OK=$?

if [ $REMOTE_OK != 0 ] ; then
    REMOTE_PART="❌"
else
    REMOTE_PART="$REMOTE_IP"
fi

SPEED=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | grep 'lastTxRate:' | grep -o '\d\+' 2>&1)

if [ $LOCAL_OK != 0 ] ; then
    SPEED_PART="❌"
    SPEED_WARNING=0
else
    SPEED_PART="$SPEED""Mbps"

    if [ "$SPEED" -lt $WARNING_SPEED ] ; then
        SPEED_WARNING=1
    else
        SPEED_WARNING=0
    fi
fi



if [ $REMOTE_OK != 0 ] ; then
    REMOTE_WARNING=1
else
    REMOTE_WARNING=0
fi

function speedcolour {
    SPEED=$1

    if [ "$SPEED" == 1 ] ; then
        echo " color=orange"
        return
    fi

    echo ""
}

function wancolour {
    WAN=$1

    if [ "$WAN" == 1 ] ; then
        echo " color=red"
        return
    fi

    echo ""
}

function topcolour {
    SPEED=$1
    WAN=$2

    if [ "$WAN" == 1 ] ; then
        wancolour "$WAN"
        return
    fi

    if [ "$SPEED" == 1 ] ; then
        speedcolour "$SPEED"
        return
    fi

    echo ""
}

echo "$LOCAL_PART | $(topcolour $SPEED_WARNING $REMOTE_WARNING) font=Courier"

echo "---"

echo "$LOCAL_PART - Local | font=Courier"
echo "$ROUTER_PART"
echo "$SPEED_PART - LAN Speed | $(speedcolour $SPEED_WARNING) font=Courier"
echo "$REMOTE_PART - WAN | $(wancolour $REMOTE_WARNING) font=Courier"

echo "---"

echo "Terminal: ifconfig| bash='ifconfig'"
echo "Terminal: Adapter Info| bash='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I'"
echo "Terminal: Wireless Scan| bash='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -s''"

echo "---"

echo "Router Web Config | href=http://$ROUTER_PART"
