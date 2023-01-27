#!/bin/bash
#
# Switch off Wifi when laptop lid is closed and force power-management-state sleeping.
# Switch on Wifi when laptop lid is opened again.
#
#  <xbar.title>wifi-toggle-by-lid</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Manuel Koch</xbar.author>
#  <xbar.author.github>manuel-koch</xbar.author.github>
#  <xbar.desc>Toggle Wifi on/off when lid gets opened/closed and force sleeping when lid gets closed.</xbar.desc>
#  <xbar.image>https://gist.github.com/manuel-koch/886cbd63ea525bbb5e0745a38693a4c0/raw/be67ebe4301ef31eb3e55173a9e1c10b82fc71e8/wifi-toggle-by-lid.jpg</xbar.image>
#  <xbar.var>string(WIFI_INTERFACE="en0"): Wifi interface name.</xbar.var>
#  <xbar.var>string(TITLE_ON=":green_apple:"): Menu title when toggle is enabled.</xbar.var>
#  <xbar.var>string(TITLE_OFF=":apple:"): Menu title when toggle is disabled.</xbar.var>

NAME=$(basename $0)
HISTORY_PATH=.${NAME}.history
MODE_FLAG_PATH=.${NAME}.mode
WIFI_OFF_FLAG_PATH=.${NAME}.wifi-off
WIFI_SWITCH_FLAG_PATH=.${NAME}.wifi-switch
cd $(dirname $0)
if [ "$1" == "enable" ] ; then
    echo "$(date): Enabling wifi toggle" >> $HISTORY_PATH
    echo "enabled" > $MODE_FLAG_PATH
    exit 0
fi
if [ "$1" == "disable" ] ; then
    echo "$(date): Disabling wifi toggle" >> $HISTORY_PATH
    echo "disabled" > $MODE_FLAG_PATH
    exit 0
fi

if test -f $MODE_FLAG_PATH && grep "enabled" $MODE_FLAG_PATH >/dev/null ; then
    echo "$TITLE_ON"
    echo "---"
    echo "disable wi-fi toggle when lid is closed/opened | refresh=true terminal=false bash=\"$0\" param1=disable"

    # check if lid is closed
    if ioreg -r -k AppleClamshellState -d 4 | grep AppleClamshellState | grep -i yes >/dev/null ; then
        # check if we already have disabled wifi
        if [ ! -f $WIFI_OFF_FLAG_PATH ] ; then
            # check if wifi is enabled
            if networksetup -getairportpower $WIFI_INTERFACE | grep -i -e 'on$' > /dev/null ; then
                # disable wifi
                echo "" > $WIFI_OFF_FLAG_PATH
                networksetup -setairportpower $WIFI_INTERFACE off
                echo "$(date): Switched OFF Wifi on interface $WIFI_INTERFACE" >> $HISTORY_PATH
                echo "$(date): Sleeping ZZzz.." >> $HISTORY_PATH
                pmset sleepnow
            fi
        fi
    else
        # check if we have disabled wifi previously and we didn't start to enable it yet
        if [ -f $WIFI_OFF_FLAG_PATH ] && [ ! -f $WIFI_SWITCH_FLAG_PATH ]; then
            # enable wifi
            echo "" > $WIFI_SWITCH_FLAG_PATH
            while networksetup -getairportpower $WIFI_INTERFACE | grep -i -e 'off$' ; do
                networksetup -setairportpower $WIFI_INTERFACE on
                sleep 1
            done
            echo "$(date): Switched ON Wifi on interface $WIFI_INTERFACE" >> $HISTORY_PATH
            rm $WIFI_OFF_FLAG_PATH
            rm $WIFI_SWITCH_FLAG_PATH
        fi
    fi
else
    echo "$TITLE_OFF"
    echo "---"
    echo "enable wi-fi toggle when lid is closed/opened | refresh=true terminal=false bash=\"$0\" param1=enable"
fi
