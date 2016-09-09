#!/bin/bash
# <bitbar.title>Check remote memory usage of Redis Server</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jimmy Spivey</bitbar.author>
# <bitbar.author.github>JimDeanSpivey</bitbar.author.github>
# <bitbar.desc>This will invoke SSH command locally and connect to a server that runs redis, where it will check the memory usage. You will likely run into this error: "ssh_askpass: exec(/usr/libexec/ssh-askpass): No such file or directory" You will need to install ssh_askpass found here: https://github.com/markcarver/mac-ssh-askpass to fix that and also enable to you to ssh from BitBar.</bitbar.desc>
# <bitbar.dependencies>ssh_askpass</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/ikCSVBq.png</bitbar.image>
#
# Dependencies:
#    ssh_askpass (https://github.com/markcarver/mac-ssh-askpass)


HOST="user@hostname"

printf 'Redis: ' 
RESULT=$(ssh $HOST "redis-cli INFO | grep -o '[0-9\.]*[A-Z]\s' | head -2")
printf "%s" "$RESULT" | head -1
echo ---
LINE1=$(echo "$RESULT" | head -n1)
LINE2=$(echo "$RESULT" | tail -n1)
echo 'Used Memory: '"$LINE1"
echo 'Peak Memory: '"$LINE2"

