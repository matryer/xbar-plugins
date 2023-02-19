#!/usr/bin/env bash

# <xbar.title>SSH Remotes</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Johannes Strodel</xbar.author>
# <xbar.author.github>John-Sane</xbar.author.github>
# <xbar.desc>Manage your Remote Servers with ease. Choose your preferred Terminal. Open a SSH connection with one click! Just list your remotes in the scripts HOSTARRAY variable :) </xbar.desc>
# <xbar.image></xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.var>select(TERMINAL="Terminal"): Choose your terminal (emulation) - iTerm2 is not supported yet. [Terminal, Hyper]</xbar.var>


# Enter your remote hosts here
# Schema: "Display Name:user@remote.adress.com"

HOSTARRAY=(
    "Example Remote 1:root@example.com"
    "Example Remote 2:user@192.168.0.2"
)


function process_arr () {
    declare -a hash=("${!1}")
    for host in "${hash[@]}"; do
        echo "${host%%:*} | bash=/usr/bin/open param1=-a param2='$TERMINAL.app'  param3=ssh://${host#*:}"
    done
}

echo "SSH | color='white'"
echo "---"

process_arr HOSTARRAY[@]
