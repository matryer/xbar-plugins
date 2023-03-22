#!/bin/bash

# <xbar.title>CPU Usage, Kill process</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alex M.</xbar.author>
# <xbar.author.github>Aleksandern</xbar.author.github>
# <xbar.desc>Shows top 3 consuming processes with opportunity to kill them.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/Aleksandern/devman/master/images/BitBarCpuUsageKill.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>


counter=1
ps c -Ao pcpu,command,pid -r | head -n 4 | awk 'NR>1'\
  | while read -r pcpu command pid ; do

    if [ "${counter}" -eq "1" ]; then
      echo "$pcpu% $command"
      echo "---"
    fi

    echo "$pcpu% $command $pid | bash='kill -9 ${pid//[!0-9]/} ; exit' terminal=true"

    counter=$((counter +1))

done


echo "---"
echo "Refresh | refresh=true terminal=false"
