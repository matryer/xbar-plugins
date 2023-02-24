#!/bin/bash
#
# <xbar.title>Power Wattage Advanced</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>matanbaruch</xbar.author>
# <xbar.author.github>matanbaruch</xbar.author.github>
# <xbar.desc>The AC Power adapter wattage consumption and current for macos</xbar.desc>
# <xbar.image>https://i.imgur.com/OpKXCQy.png</xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl>github.matan-baruch.com</xbar.abouturl>

power_consumption=$(ioreg -rw0 -c AppleSmartBattery | grep BatteryData | grep -o '"SystemPower"=[0-9]*' | cut -c 16- | xargs -I %  lldb --batch -o "print/f %" | grep -o '$0 = [0-9.]*' | cut -c 6-9)

power_wattage=$(ioreg -rw0 -c AppleSmartBattery | grep BatteryData | grep -o '"AdapterPower"=[0-9]*' | cut -c 16- | xargs -I %  lldb --batch -o "print/f %" | grep -o '$0 = [0-9.]*' | cut -c 6-9)

if [ -z "$power_wattage" ]
then
  echo "🔋"
else
  echo "${power_consumption}W🔌${power_wattage}W"
fi

echo "---"
echo "Refresh... | refresh=true"
