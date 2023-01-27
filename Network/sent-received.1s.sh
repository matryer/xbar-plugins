#!/usr/bin/env bash

# <xbar.title>Network Sent/Received Totals</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Paul W. Rankin</xbar.author>
# <xbar.author.github>rnkn</xbar.author.github>
# <xbar.desc>Displays total sent and received wifi data for current sesssion.</xbar.desc>
# <xbar.image>https://i.imgur.com/KxXfbKn.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

# BitBar Network Sent/Received plugin

INTERFACE="en0"
TMPFILE="${TMPDIR}/bitbar-sent-received"

if [[ -r "${TMPFILE}" ]]
then
    read -ra offsets < "${TMPFILE}"
else
    echo "0 0" > "${TMPFILE}"
    offsets=(0 0)
fi

totals=$(netstat -I "${INTERFACE}" -b)

if [[ $1 == reset ]]
then
    awk 'NR-1==1 {print $7, $10}' <<< "${totals}" > "${TMPFILE}"
fi

if [[ $1 == undo_reset ]]
then
    rm "${TMPFILE}"
fi

in_bytes=$(awk 'NR-1==1 {print $7}' <<< "${totals}")
offset_in_bytes=$(bc <<< "${in_bytes} - ${offsets[0]}")

out_bytes=$(awk 'NR-1==1 {print $10}' <<< "$totals")
offset_out_bytes=$(bc <<< "${out_bytes} - ${offsets[1]}")


function convert_bytes {
    if [[ $1 -lt 1000000000 ]]
    then
        printf "%0.1f MB" "$(bc <<< "scale = 1; $1 / 1048576")"
    else
        printf "%0.2f GB" "$(bc <<< "scale = 2; $1 / 1073741824")"
    fi
}

# shellcheck disable=SC2086
echo "▼ $(convert_bytes ${offset_in_bytes}) ▲ $(convert_bytes ${offset_out_bytes})"
echo "---"
echo "Reset Counters | bash='$0' param1=reset terminal=false"
echo "Restore Counters | alternate=true bash='$0' param1=undo_reset terminal=false"
echo "---"
echo "Refresh | refresh=true terminal=false"
