#!/usr/bin/env bash

# <xbar.title>strongSwan Toggle</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Matteo Carnelos</xbar.author>
# <xbar.author.github>matteocarnelos</xbar.author.github>
# <xbar.desc>Simple start/stop toggle for the strongSwan VPN</xbar.desc>
# <xbar.image>https://github.com/user-attachments/assets/dec9bed8-28f6-45db-aa48-18078e6bafab</xbar.image>
# <xbar.dependencies>strongswan</xbar.dependencies>
# <xbar.abouturl>https://github.com/matryer/xbar-plugins/blob/main/Network/strongswan-toggle.1m.sh</xbar.abouturl>

# <swiftbar.hideAbout>true</swiftbar.hideAbout>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>true</swiftbar.hideLastUpdated>
# <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
# <swiftbar.hideSwiftBar>true</swiftbar.hideSwiftBar>
# <swiftbar.refreshOnOpen>true</swiftbar.refreshOnOpen>

# **** NOTE ****
# To avoid having to enter the root password, allow passwordless sudo for your
# user on ipsec by adding the following line to the sudoers file:
# <your-username> ALL=(ALL) NOPASSWD: /usr/local/bin/ipsec

export PATH="/usr/local/bin:/usr/bin:$PATH"
STRONGSWAN_ICON="iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAA6ElEQVR4nNXSzyqFURQF8N8EVyFTZhhLSuaUP8kDKA/g5gm8gbkyoEwUZWCsTBQpA8/AwIQZE0V0tWt/dfq6vju+u9Zgr73WPvvsc+i3aGEN872E0zjAHa6xnfwVOonN/8wL+CiEgdOsHWX+iXO06+YRPNfMgXfMYgAX+MJqt9P3upgrxFQbGMQtXjFeX9BbQ4PAN6awnPlW2WCnEEb3MxzivuB/MIPFzHcr8xhekgzjcO1qK3goNn+S2rlIJnGTxBOGGp53AsepvazI32LE/QZzK7ffyZcYrQqPBZYaGkTEP1jvoem3+AO2G0kOLwX/JAAAAABJRU5ErkJggg=="

case "$1" in
    connect)
        sudo ipsec start
        sleep 1
        ;;
    disconnect)
        sudo ipsec stop
        ;;
esac

if [[ $(sudo ipsec status) =~ \((.*)\) ]]; then
    echo "⬆ | color=green templateImage=$STRONGSWAN_ICON"
    echo "---"
    echo "${BASH_REMATCH[1]} | disabled=true"
    echo "Stop strongSwan | bash='$0' param1=disconnect terminal=false refresh=true"
else
    echo "| templateImage=$STRONGSWAN_ICON"
    echo "---"
    echo "Start strongSwan | bash='$0' param1=connect terminal=false refresh=true"
fi

echo "Open Configuration Folder... | href='file:///usr/local/etc'"
