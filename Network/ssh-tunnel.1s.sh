#!/usr/bin/env bash
#
# Easily start/stop a background SSH forwarding connection
# The list of hosts are extracted from ~/.ssh/config by default
#
# To connect to your favorit host, just click the host name
# To disconnect the host, click the host name that is displayed as "(connecting)"
#
# <xbar.title>SSH Tunnel</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>mutsune</xbar.author>
# <xbar.author.github>mutsune</xbar.author.github>
# <xbar.desc>Easily start/stop a background SSH forwarding connection.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/wiki/mutsune/bitbar-plugins/images/ssh-tunnel.png</xbar.image>
#

if pgrep -qf "ssh -fN"; then
    echo ":earth_americas:"
else
    echo ":globe_with_meridians:"
fi
echo "---"

# get host names that are specified forwarding options
function hosts() {
    awk '
        $1 == "Host" {
            host = $2;
            next;
        }
        $1 == "DynamicForward" || $1 == "LocalForward" {
            print host;
        }
    ' "$1" | uniq
}

for h in $(hosts ~/.ssh/config); do
    if pgrep -qf "ssh -fN ${h}"; then
        echo "(connecting) ${h} | color=indianred bash=/usr/bin/pkill param1='-f' param2='\"ssh -fN ${h}\"' terminal=false"
    else
        echo "${h} | bash=/usr/bin/ssh param1='-fN' param2=${h} terminal=false"
    fi
done
