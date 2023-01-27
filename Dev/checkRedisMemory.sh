#!/bin/bash
# <xbar.title>Check remote memory usage of Redis Server</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jimmy Spivey</xbar.author>
# <xbar.author.github>JimDeanSpivey</xbar.author.github>
# <xbar.desc>This will invoke SSH command locally and connect to a server that runs redis, where it will check the memory usage. You will likely run into this error: "ssh_askpass: exec(/usr/libexec/ssh-askpass): No such file or directory" You will need to install ssh_askpass found here: https://github.com/markcarver/mac-ssh-askpass to fix that and also enable to you to ssh from BitBar.</xbar.desc>
# <xbar.dependencies>ssh_askpass</xbar.dependencies>
# <xbar.image>http://i.imgur.com/ikCSVBq.png</xbar.image>
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

