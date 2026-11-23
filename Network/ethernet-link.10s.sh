#!/bin/bash
# <xbar.title>Ethernet Link Monitor</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.desc>Local Ethernet link speed, refreshed every 10 seconds.</xbar.desc>
# <xbar.author>goldmahn</xbar.author>
# <xbar.author.github>goldmahn</xbar.author.github>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/goldmahn/xbar-plugins/blob/ethernet-link-monitor/Network/ethernet-link.10s.sh</xbar.abouturl>
# <xbar.image>https://raw.githubusercontent.com/goldmahn/xbar-plugins/ethernet-link-monitor/.images/ethernet-link.png</xbar.image>
# <xbar.var>string(VAR_INTERFACE="en0"): Ethernet interface to monitor (for example en0 or en5).</xbar.var>
#
# INSTALL: Copy this executable file to your SwiftBar or xbar Plugin Folder.
# Keep .10s in the filename for a ten-second refresh interval.
# Set VAR_INTERFACE in the app's plugin settings, or edit the default below.
# Find Ethernet interface names with: /usr/sbin/networksetup -listallhardwareports
# macOS /bin/bash compatible; no additional packages are required.
#
# Green = 1000 Mbps; yellow = 100 Mbps, other speeds, or unavailable status;
# red = inactive link. The menu bar is dot-only; details are in the dropdown.
# Reads local ifconfig information; sends no network traffic or speed tests.
# This is only the Mac-to-peer link, NOT internet throughput or downstream
# cable health. A slower link between switches can coexist with a green dot.
#
# State and transition logs are local and persist across restarts, under
# ~/Library/Application Support/SwiftBar/EthernetLink/ (also when using xbar).
# The first observation establishes a baseline and creates no log entry.
# Timestamps show detection time, not the exact physical change time.
# Changes while asleep, while the app is closed, or between polls can be missed.
# Logs are not rotated automatically. Remove the plugin to stop monitoring;
# optionally delete its state and log files to erase history.
# Preview image is an illustration with sample values, not a live screenshot.
# Developed with OpenAI Codex assistance.
export LC_ALL=C
umask 077
interface=${VAR_INTERFACE:-en0}
# Only macOS-style interface names may become filenames or command arguments.
if [[ ! "$interface" =~ ^[a-zA-Z][a-zA-Z0-9]*[0-9]$ ]]; then
    printf '%s\n' '🟡' '---' 'Invalid interface setting' 'Use an interface name such as en0 or en5.'
    exit 0
fi
data_dir="$HOME/Library/Application Support/SwiftBar/EthernetLink"
state_file="$data_dir/$interface.state"
log_file="$data_dir/$interface-transitions.log"
now=$(/bin/date '+%Y-%m-%d %H:%M:%S %z')

raw=$(/sbin/ifconfig "$interface" 2>/dev/null)
result=$?
status=$(printf '%s\n' "$raw" | /usr/bin/awk '/^[[:space:]]*status:/ {print $2; exit}')
media=$(printf '%s\n' "$raw" | /usr/bin/sed -n 's/^[[:space:]]*media: //p')
speed=$(printf '%s\n' "$media" | /usr/bin/sed -nE 's/.*\(([0-9]+)base.*/\1/p')
ipv4=$(printf '%s\n' "$raw" | /usr/bin/awk '$1 == "inet" {print $2; exit}')

if [ "$result" -ne 0 ]; then
    current="unavailable"; title="🟡 $interface unavailable"
elif [ "$status" = inactive ]; then
    current="disconnected"; title="🔴 Disconnected"
elif [ "$status" = active ]; then
    current="active:${speed:-unknown}"
    case "$speed" in
        1000) title="🟢 1000 Mbps" ;;
        100) title="🟡 100 Mbps" ;;
        '') title="🟡 Speed unknown" ;;
        *) title="🟡 $speed Mbps" ;;
    esac
else
    current="unknown"; title="🟡 Link unknown"
fi

# Only changes in connection state or negotiated speed count as transitions.
# The first observation establishes a baseline without adding a log entry.
last_change="None observed"
first_seen="$now"
warning=""
if /bin/mkdir -p "$data_dir"; then
    previous=""
    if [ -f "$state_file" ]; then
        { IFS= read -r previous; IFS= read -r last_change; IFS= read -r first_seen; } < "$state_file"
    fi
    if [ -n "$previous" ] && [ "$previous" != "$current" ]; then
        last_change="$now"
        if ! printf '%s\t%s -> %s\n' "$now" "$previous" "$current" >> "$log_file"; then
            warning="Could not append transition log"
        fi
    fi
    if [ "$previous" != "$current" ]; then
        temp=$(/usr/bin/mktemp "$data_dir/.state.XXXXXX")
        if [ -n "$temp" ]; then
            if ! { printf '%s\n%s\n%s\n' "$current" "$last_change" "$first_seen" > "$temp" && /bin/mv -f "$temp" "$state_file"; }; then
                warning="Could not save monitoring state"
            fi
            /bin/rm -f "$temp"
        else
            warning="Could not save monitoring state"
        fi
    fi
else
    warning="Local monitoring storage unavailable"
fi

printf '%s\n' "${title%% *}" '---' "$title"
printf 'Interface: %s\n' "$interface"
printf 'Link: %s\n' "$current"
printf 'Media: %s\n' "${media:-Unavailable}"
printf 'IPv4: %s\n' "${ipv4:-None}"
printf 'Last status change: %s\n' "$last_change"
printf 'Monitoring since: %s\n' "$first_seen"
printf 'Last checked: %s\n' "$now"
printf '%s\n' '---' 'Refresh now | refresh=true'
printf 'Transition log: %s\n' "$log_file"
if [ -n "$warning" ]; then printf 'Warning: %s\n' "$warning"; fi
printf '%s\n' '---' 'Measures this Mac’s Ethernet link only.' 'Does not measure internet speed or downstream links.'
