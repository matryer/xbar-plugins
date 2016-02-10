#!/bin/bash
#
# <bitbar.title>Virtualbox VM status</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Johan Bloemberg</bitbar.author>
# <bitbar.author.github>aequitas</bitbar.author.github>
# <bitbar.desc>Show current number of running Virtualbox VM's. Allow to shut them all down.</bitbar.desc>

vms="$(/usr/bin/VBoxManage list runningvms | cut -f2 -d\")"
vms_count=$(echo "$vms" | wc -l)
vms_count=$((vms_count))

if test ! -z "$1"; then
    if test -z "$2"; then
        echo $vms | xargs -I% /usr/bin/VBoxManage controlvm % $1
    else
        /usr/bin/VBoxManage controlvm $2 $1
    fi
    exit
fi

if test -z "$vms"; then
    echo "V"
else
    echo "V $vms_count"
    echo '---'
    echo "Poweroff all VM's | bash=$0 param1=poweroff terminal=false"
    echo '---'
    for vm in $vms;do
        echo "Poweroff $vm | bash=$0 param1=poweroff param2=$vm terminal=false"
    done
fi
