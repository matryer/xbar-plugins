#!/usr/bin/env bash
# <bitbar.title>MySQL server status</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Kenji Akiyama</bitbar.author>
# <bitbar.author.github>artifactsauce</bitbar.author.github>
# <bitbar.desc>Show the status of MySQL server installed by Homebrew on localhost and manage server boot with shortcut menus</bitbar.desc>
# <bitbar.image>http://i.imgur.com/Y85ENFb.png</bitbar.image>
# <bitbar.dependencies>bash,mysql</bitbar.dependencies>

set -eu

# Change here depending on your preference
MENUBAR_ICON_ENABLED=":dolphin:"
MENUBAR_ICON_DISABLED=":sleepy:"
STATUS_ITEM_COLOR="green"
DISABLED_ITEM_COLOR="#C0C0C0"

# Below is no need to change basically.
SERVER_CMD="/usr/local/bin/mysql.server"

SUBCMD_START="start"
SUBCMD_STOP="stop"
SUBCMD_RESTART="restart"
SUBCMD_RELOAD="reload"
SUBCMD_F_RELOAD="force-reload"
SUBCMD_STATUS="status"

if $SERVER_CMD $SUBCMD_STATUS | grep -Fq 'SUCCESS'; then
  IS_SERVER_RUNNING=true
  echo "$MENUBAR_ICON_ENABLED"
else
  IS_SERVER_RUNNING=false
  echo "$MENUBAR_ICON_DISABLED"
fi

echo "---"
echo "MySQL Server"

# Server Status from `mysqladmin status`
if $IS_SERVER_RUNNING; then
  echo "---"
  LF=$'\\\x0A' # return code
  /usr/local/bin/mysqladmin -u root status | sed -e "s/$/  /" | sed -e "s/  / \| color=$STATUS_ITEM_COLOR $LF/g"
fi

# Server management shortcuts
echo "---"
if $IS_SERVER_RUNNING; then
  echo "Start | color=$DISABLED_ITEM_COLOR"
  echo "Stop | bash=$SERVER_CMD param1=$SUBCMD_STOP refresh=true terminal=false"
  echo "Restart | bash=$SERVER_CMD param1=$SUBCMD_RESTART refresh=true terminal=false"
  echo "Reload | bash=$SERVER_CMD param1=$SUBCMD_RELOAD refresh=true terminal=false"
  echo "Force-reload | bash=$SERVER_CMD param1=$SUBCMD_F_RELOAD refresh=true terminal=false"
else
  echo "Start | bash=$SERVER_CMD param1=$SUBCMD_START refresh=true terminal=false"
  echo "Stop | color=$DISABLED_ITEM_COLOR"
  echo "Restart | color=$DISABLED_ITEM_COLOR"
  echo "Reload | color=$DISABLED_ITEM_COLOR"
  echo "Force-reload | color=$DISABLED_ITEM_COLOR"
fi

echo "---"
echo "Refresh | refresh=true color=$DISABLED_ITEM_COLOR"
