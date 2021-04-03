#!/bin/bash

# <xbar.title>Mac App Store</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>iosdeveloper</xbar.author>
# <xbar.author.github>iosdeveloper</xbar.author.github>
# <xbar.desc>Lists available updates from the Mac App Store using mas-cli (https://github.com/argon/mas). Based on brew-updates.1h.sh</xbar.desc>
# <xbar.image>http://i.imgur.com/yPeXDAj.png</xbar.image>
# <xbar.dependencies>mas</xbar.dependencies>
# <xbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/System/mas.1d.sh</xbar.abouturl>

if test "$(which /usr/local/bin/mas)" 2> /dev/null; then
  UPDATES=$(/usr/local/bin/mas outdated);
  UPDATE_COUNT=$(echo "$UPDATES" | grep -c '[^[:space:]]');
  if test "$UPDATE_COUNT" -gt 0; then
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
