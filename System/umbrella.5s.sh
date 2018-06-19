#!/usr/bin/env bash
#
## <bitbar.title>UmbrellaBeGone</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# # <bitbar.author>Jeff Levensailor</bitbar.author>
# # <bitbar.author.github>levensailor</bitbar.author.github>
# # <bitbar.desc>Enable/Disable Umbrella with Status on Menubar</bitbar.desc>
# # <bitbar.dependancies>umbrella.15s.sh</bitbar.dependancies>

# Dependancies:
# If you want seamless integration, add NOPASSWD=ALL to admin account in /etc/sudoers file

export PATH="$PATH:/usr/local/bin"
umbrella_status() {
    status=$(sudo launchctl list | grep com.cisco.anyconnect.vpnagentd)
    if [ "$status" != "" ]; then
        echo "👆"
    elif [ "$status" = "" ]; then
        echo "👇"
    else echo "err"
    fi
    #echo "$status"
}

enable_umbrella() {
    sudo launchctl load /Library/LaunchDaemons/com.cisco.anyconnect.vpnagentd.plist && umbrella_status
}
disable_umbrella() {
    sudo launchctl unload /Library/LaunchDaemons/com.cisco.anyconnect.vpnagentd.plist && umbrella_status
}

if [ "${1}" == "enable" ]; then
    enable_umbrella
    

elif [ "${1}" == "disable" ]; then
    disable_umbrella

else
    umbrella_status
    echo "---"
    echo "---"
    echo "Enable Umbrella | bash=$0 param1=enable terminal=false"
    echo "---"
    echo "Disable Umbrella | bash=$0 param1=disable terminal=false"
fi
