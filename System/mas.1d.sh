#!/bin/bash

# <bitbar.title>Mac App Store</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>iosdeveloper</bitbar.author>
# <bitbar.author.github>iosdeveloper</bitbar.author.github>
# <bitbar.desc>Lists available updates from the Mac App Store using mas-cli (https://github.com/argon/mas). Based on brew-updates.1h.sh</bitbar.desc>
# <bitbar.image>http://i.imgur.com/yPeXDAj.png</bitbar.image>
# <bitbar.dependencies>mas</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/System/mas.1d.sh</bitbar.abouturl>

if test `which /usr/local/bin/mas` 2> /dev/null; then
  UPDATES=$(/usr/local/bin/mas outdated);
  UPDATE_COUNT=$(echo "$UPDATES" | grep -c '[^[:space:]]');
  if test $UPDATE_COUNT -gt 0; then
    echo "↓$UPDATE_COUNT | dropdown=false"
    echo "---";
    if [ -n "$UPDATES" ]; then
      echo "Upgrade all | bash=/usr/local/bin/mas param1=upgrade terminal=false refresh=true"
      echo "$UPDATES" | awk '{itemIdentifier = $1; $1 = ""; print $0 " | bash=/usr/local/bin/mas param1=install param2=" itemIdentifier " terminal=false refresh=true"; }'
    fi
  fi
else
  echo "mas not installed"
fi
