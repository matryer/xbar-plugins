#!/usr/bin/env bash
# <bitbar.title>PostgreSQL server status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kenji Akiyama</bitbar.author>
# <bitbar.author.github>artifactsauce</bitbar.author.github>
# <bitbar.desc>Show the status of PostgreSQL server installed by Homebrew on localhost and manage server boot with shortcut menus</bitbar.desc>
# <bitbar.image>http://i.imgur.com/l5E4yg8.png</bitbar.image>
# <bitbar.dependencies>bash,perl,postgresql</bitbar.dependencies>

# TODO Selecting some menus will return warnings because the process has not been completed.

set -eu

# Change here depending on your preference
MENUBAR_ICON_ENABLED=":elephant:"
MENUBAR_ICON_DISABLED=":sleepy:"
STATUS_ITEM_COLOR="green"
DISABLED_ITEM_COLOR="#C0C0C0"

# Below is no need to change basically.
SERVER_CMD="/usr/local/bin/pg_ctl"
PGDATA="/usr/local/var/postgres"

SUBCMD_START="start"
SUBCMD_STOP="stop"
SUBCMD_RESTART="restart"
SUBCMD_RELOAD="reload"
SUBCMD_STATUS="status"

if $SERVER_CMD -D $PGDATA $SUBCMD_STATUS | grep -Fq 'server is running'; then
  IS_SERVER_RUNNING=true
  echo "$MENUBAR_ICON_ENABLED"
else
  IS_SERVER_RUNNING=false
  echo "$MENUBAR_ICON_DISABLED"
fi

echo "---"
echo "PostgreSQL Server"

# Server Status from PostgreSQL's `pg_stat_database` table
if $IS_SERVER_RUNNING; then
  /usr/local/bin/psql -U postgres -w -q -A -F $'\x09' -c "select * from pg_stat_database;" | perl -nle 'BEGIN { $_=<>; chomp; @headers = split "\t"} last if eof(); @records{@headers} = split "\t"; next if $records{datname} =~/^template/; print "---"; print"$_: $records{$_} | color='$STATUS_ITEM_COLOR'" for @headers'
fi

# Server management shortcuts
echo "---"
if $IS_SERVER_RUNNING; then
  echo "Start | color=$DISABLED_ITEM_COLOR"
  echo "Stop | bash=$SERVER_CMD param1=-D param2=$PGDATA param4=$SUBCMD_STOP refresh=true terminal=false"
  echo "Restart | bash=$SERVER_CMD param1=-D param2=$PGDATA param4=$SUBCMD_RESTART refresh=true terminal=false"
  echo "Reload | bash=$SERVER_CMD param1=-D param2=$PGDATA param4=$SUBCMD_RELOAD refresh=true terminal=false"
else
  echo "Start | bash=$SERVER_CMD param1=-D param2=$PGDATA param4=$SUBCMD_START refresh=true terminal=false"
  echo "Stop | color=$DISABLED_ITEM_COLOR"
  echo "Restart | color=$DISABLED_ITEM_COLOR"
  echo "Reload | color=$DISABLED_ITEM_COLOR"
fi

echo "---"
echo "Refresh | refresh=true color=$DISABLED_ITEM_COLOR"
