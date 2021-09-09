#!/bin/bash
#
# <xbar.title>Power Wattage</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>vxider</xbar.author>
# <xbar.author.github>vxider</xbar.author.github>
# <xbar.desc>The AC power adapter wattage for macos</xbar.desc>
# <xbar.image>https://i.imgur.com/4N0xk1g.jpg</xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

power_wattage="$(system_profiler SPPowerDataType | grep "Wattage (W)" | awk "{print \$3\"W\"}")"

if [ -z "$power_wattage" ]
then
  echo "🔋"
else
  echo "🔌${power_wattage}"
fi

echo "---"
echo "Refresh... | refresh=true"
