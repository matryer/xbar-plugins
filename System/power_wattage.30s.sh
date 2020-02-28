#!/bin/bash
#
# <bitbar.title>Power Wattage</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>vxider</bitbar.author>
# <bitbar.author.github>vxider</bitbar.author.github>
# <bitbar.desc>The AC power adapter wattage for macos</bitbar.desc>
# <bitbar.image>https://i.imgur.com/4N0xk1g.jpg</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

power_wattage="$(/usr/sbin/system_profiler SPPowerDataType | grep "Wattage (W)" | awk "{print \$3\"W\"}")"

if [ -z "$power_wattage" ]
then
  echo "🔋" 
else
  echo "🔌${power_wattage}"
fi

echo "---"
echo "Refresh... | refresh=true"