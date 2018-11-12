#!/bin/bash

# <bitbar.title>Redis - Memcached</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Hugo Defrane</bitbar.author>
# <bitbar.author.github>koleror</bitbar.author.github>
# <bitbar.desc>This plugin will start and stop a memcached and redis server</bitbar.desc>
# <bitbar.image>http://oi67.tinypic.com/28u1y1g.jpg</birtbar.image>
# <bitbar.dependencies></bitbar.dependencies>

REDIS_PID=$(/usr/bin/pgrep redis-server)
MEMCACHED_PID=$(/usr/bin/pgrep memcached)

# GENERAL COMMANDS
if [ "$1" = 'start_all' ]; then
  # start all
   /usr/local/bin/redis-server &
   /usr/local/bin/memcached &
fi

if [ "$1" = 'restart_all' ]; then
  /usr/bin/killall memcached
  /usr/local/bin/redis-cli shutdown NOSAVE
   /usr/local/bin/redis-server &
   /usr/local/bin/memcached &
fi
if [ "$1" = 'stop_all' ]; then
  /usr/bin/killall memcached
  /usr/local/bin/redis-cli shutdown NOSAVE
fi


# MEMCACHED
if [ "$1" = 'start_memcached' ]; then
  # stop all previous processes
  /usr/local/bin/memcached &
fi
if [ "$1" = 'stop_memcached' ]; then
  # send terminate signal to all processes of program 'redis-server'
  /usr/bin/killall memcached
fi

# REDIS
if [ "$1" = 'start_redis' ]; then
  # stop all previous processes
  echo "start"
  /usr/local/bin/redis-server &
fi
if [ "$1" = 'stop_redis' ]; then
  /usr/local/bin/redis-cli shutdown NOSAVE
fi
if [ "$1" = 'flush_redis' ]; then
  /usr/local/bin/redis-cli FLUSHALL
fi


# MENU
echo "⚙️"
echo '---'
if [ "$MEMCACHED_PID" = "" ] && [ "$REDIS_PID" = "" ]; then
  echo "Start all | bash='$0' param1=start_all terminal=false refresh=true"
else
  echo "Restart all | bash='$0' param1=restart_all terminal=false refresh=true"
fi
if [ "$MEMCACHED_PID" != "" ] || [ "$REDIS_PID" != "" ]; then
  echo "Stop all | bash='$0' param1=stop_all terminal=false refresh=true"
fi

echo '---'
if [ "$MEMCACHED_PID" = "" ]; then
    echo "Start memcached | bash='$0' param1=start_memcached terminal=false refresh=true"
else
    echo "Stop memcached | bash='$0' param1=stop_memcached terminal=false refresh=true"
fi
echo '---'
if [ "$REDIS_PID" = "" ]; then
    echo "Start redis server | bash='$0' param1=start_redis terminal=false refresh=true"
else
    echo "Flush Redis | bash='$0' param1=flush_redis terminal=false"
    echo "Stop Redis | bash='$0' param1=stop_redis terminal=false refresh=true"
fi
