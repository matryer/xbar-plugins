#!/bin/sh
#
# <bitbar.title>Virtualbox VM status</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Johan Bloemberg</bitbar.author>
# <bitbar.author.github>aequitas</bitbar.author.github>
# <bitbar.desc>Show current number of running Virtualbox VM's. Allow to shut them all down.</bitbar.desc>
# <bitbar.image>https://gist.githubusercontent.com/aequitas/7c57111556900aae6178/raw/76e4e457d700d31af6b2f32cbbb206ed1668b511/virtualbox.png</bitbar.image>

if test -x /usr/local/bin/VBoxManage;then
    VBOX=/usr/local/bin/VBoxManage
elif test -x /usr/bin/VBoxManage;then
    VBOX=/usr/bin/VBoxManage
else
    echo "Failed to find VBoxManage command in default locations."
    exit 1
fi

vms="$(${VBOX} list runningvms | cut -f2 -d\")"
vms_count=$(echo "$vms" | wc -l)
vms_count=$((vms_count))

if test ! -z "$1"; then
    if test -z "$2"; then
        echo "$vms" | xargs -I% ${VBOX} controlvm % "$1"
    else
        ${VBOX} controlvm "$2" "$1"
    fi
    exit
fi

if test -z "$vms"; then
    echo "V"
else
    echo "V $vms_count"
    echo '---'
    echo "Poweroff all VM's | bash='$0' param1=poweroff terminal=false"
    echo "ACPI Shutdown all VM's | bash='$0' param1=acpipowerbutton terminal=false"
    echo '---'
    for vm in $vms;do
        echo "Poweroff $vm | bash='$0' param1=poweroff param2=$vm terminal=false"
        echo "ACPI Shutdown $vm | bash='$0' param1=acpipowerbutton param2=$vm terminal=false"
    done
fi
