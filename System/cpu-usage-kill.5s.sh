#!/bin/bash

# <bitbar.title>CPU Usage, Kill process</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Alex M.</bitbar.author>
# <bitbar.author.github>Aleksandern</bitbar.author.github>
# <bitbar.desc>Shows top 3 consuming processes with opportunity to kill them.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/Aleksandern/devman/master/images/BitBarCpuUsageKill.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>


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
echo "Refresh | refresh=true terminal=false root=true"