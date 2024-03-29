#!/bin/bash

# <xbar.title>iCloud Private Relay Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ask Adam</xbar.author>
# <xbar.author.github>arice</xbar.author.github>
# <xbar.desc>This plugin will  indicate whether iCloud Private Relay is active by comparing your WAN IP to Apple's list of iCloud Private Relay egress servers.</xbar.desc>
# <xbar.image>https://askadam.io/imgs/xbar-icpr-image.png</xbar.image>
# <xbar.dependencies>curl</xbar.dependencies>

# based on CodinCafe's "My External IP" plugin, thanks for the code!

if [ $(curl -LI http://google.com -o /dev/null -w '%{http_code}\n' -s) == "200" ]; then
# we have an internet connection
    wanIP=$(curl -s https://ipinfo.io/ip)
    iCloudPrivateRelayIP=$(curl -4 -s http://ifconfig.me)
    # using http and ifconfig.me seems to reliably return the iCloud Private Relay IP if Private Relay is enabled



    # now, get the public list of iCloud Private Relay egresses if it's stale

    mkdir -p "${TMPDIR}io.askadam"
    egresses="egress-ip-ranges.csv"
    iCloudEgressList="${TMPDIR}io.askadam/$egresses"
    
    # get a new copy of the egress ips every few days rather than every run
    if [ ! -f "$iCloudEgressList" ]; then
        curl --location --silent "https://mask-api.icloud.com/egress-ip-ranges.csv" -o "$iCloudEgressList"         
    elif [[ $(find "$iCloudEgressList" -mtime +3 -print) ]]; then # it's older than 3 days; delete and get a new one
        rm iCloudEgressList
        curl --location --silent "https://mask-api.icloud.com/egress-ip-ranges.csv" -o "$iCloudEgressList"         
    fi
   
    ICPResult_in_EgressList=$(fgrep "$iCloudPrivateRelayIP" "$iCloudEgressList")
    # test if our potential relay IP shows up in the list we just got from Apple
    if [ ICPResult_in_EgressList != "" ]; then
        Region=$(echo $ICPResult_in_EgressList | awk -F, '{print $4}')
        if [ "$Region" = "" ]; then
            Region="Region unlisted"
        fi
    fi
    # here, we set the menu bar icon to either a green sparkle (yay safe!) or a red flag (boo danger!)
    if [ "$wanIP" != "$iCloudPrivateRelayIP" ] && [ ICPResult_in_EgressList != "" ]; then
        ICP_active=true
        echo "❇️️"
    else
        ICP_active=""
        echo "🚩"
    fi
else
    wanresult_string="No Internet Connection"
    echo "❌️"
fi

    # build the strings to use for the menu output
    wanresult_string="Public IP: $wanIP | bash='/bin/bash' param1='-c' param2='/bin/echo $wanIP | pbcopy' terminal=false"
    icprresult_string="iCloud Relay IP: $iCloudPrivateRelayIP ($Region) | bash='/bin/bash' param1='-c' param2='/bin/echo $iCloudPrivateRelayIP | pbcopy' terminal=false"


d=$(date +"%A, %b %d, %I:%M %p")

echo '---' 

# operational code; commented out for screenshot
echo "$wanresult_string"
if [ "$ICP_active" = true ]; then
    echo "$icprresult_string"
else
    echo "iCloud Private Relay is NOT active"
fi

# menu items for screenshot
# echo "Public IP: 25.0.0.7"
# echo "iCloud Relay IP: 172.224.224.7 (London)"
# echo "($Region)"
echo "Checked: $d"
echo "Apple ID Preferences... | bash='/bin/bash' param1='-c' param2='/usr/bin/open /System/Library/PreferencePanes/AppleIDPrefPane.prefPane/' terminal=false"
# echo $ICPResult_in_EgressList
echo ---
echo "Refresh... | refresh=true"
