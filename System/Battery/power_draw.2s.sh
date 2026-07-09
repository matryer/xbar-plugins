#!/usr/bin/env bash

# <xbar.title>Power Draw</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>moogly81</xbar.author>
# <xbar.author.github>moogly81</xbar.author.github>
# <xbar.desc>Discreet power monitor. Nothing in the menu bar on battery; a plug icon when on AC. The dropdown shows the live system power draw (in watts) plus the connected charger/adapter rating.</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>

# xbar can run with a minimal PATH; set a sane one so the tools below resolve.
export PATH=/usr/bin:/bin:/usr/sbin:/sbin:$PATH

# ---- Power source (pmset) ----
source=$(pmset -g batt | head -1 | grep -Eo "'[^']*'" | tr -d "'")

# ---- ioreg battery blob (reused) ----
ioreg_batt=$(ioreg -rn AppleSmartBattery 2>/dev/null)

# ---- Live power draw (mW -> W) ----
# `SystemPowerIn` is the power coming IN from the adapter; it is 0 on battery.
# On battery, the live draw is `BatteryPower`, a signed value stored as an
# unsigned 64-bit int (negative == discharging). We unwrap it and take the
# magnitude, so `draw_w` is always the real live wattage in either state.
U64=18446744073709551616   # 2^64, for unwrapping "negative" unsigned ints
draw_w=""
sys_mw=$(echo "$ioreg_batt" | grep -Eo '"SystemPowerIn"=[0-9]+' | grep -Eo '[0-9]+' | head -1)
if [ -n "$sys_mw" ] && [ "$sys_mw" -gt 0 ]; then
    draw_w=$(awk "BEGIN{printf \"%.0f\", $sys_mw/1000}")
else
    batt_mw=$(echo "$ioreg_batt" | grep -Eo '"BatteryPower"=[0-9]+' | grep -Eo '[0-9]+' | head -1)
    if [ -n "$batt_mw" ]; then
        draw_w=$(awk "BEGIN{p=$batt_mw; if(p> $U64/2) p=p-$U64; p=(p<0?-p:p); if(p>0) printf \"%.0f\", p/1000}")
    fi
fi
[ "$draw_w" = "0" ] && draw_w=""

# ---- Charger rating (W) + name ----
charger_w=$(echo "$ioreg_batt" | grep -Eo '"Watts"=[0-9]+' | grep -Eo '[0-9]+' | head -1)
charger_name=$(echo "$ioreg_batt" | grep -Eo '"Name"="[^"]*"' | sed -E 's/.*="([^"]*)"/\1/' | head -1)

# ---- Plugged state ----
plugged=0
echo "$source" | grep -qi "AC Power" && plugged=1

# =========================================================
# Menu bar line
#   - On battery : nothing (blank item; the dropdown still opens on click)
#   - On AC      : a discreet plug icon only
#
# The plug is an embedded TEMPLATE image (black pixels + alpha). xbar renders
# templateImage= adaptively: white on a dark menu bar, black on a light one,
# with no background box. Base64 is a 48x28 (2x/144dpi) render of the
# `powerplug.fill` SF Symbol, so the plugin stays fully self-contained.
PLUG_B64="iVBORw0KGgoAAAANSUhEUgAAADAAAAAcCAYAAAAnbDzKAAAAAXNSR0IArs4c6QAAAGxlWElmTU0AKgAAAAgABAEaAAUAAAABAAAAPgEbAAUAAAABAAAARgEoAAMAAAABAAIAAIdpAAQAAAABAAAATgAAAAAAAACQAAAAAQAAAJAAAAABAAKgAgAEAAAAAQAAADCgAwAEAAAAAQAAABwAAAAAtSxHDwAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAdVJREFUWAntmM8rBGEYx9ePXEQclLRsysHF1UH8AU4cxMVx8wc4uzoqxdHBTVw4uDg7cJEiojhIpEiKogifZzM1zT4z7+x4dme39q3PzszzPPs833nnmXd2NpOpj3RnoKHM5fvJPwy90Ar+et8c38MOPEHVjCaU5OEEfmLwSEw3VMUYRMUxxBHuj5lPqr456ReV70mr7EGH4nOZsq6AML/VCeQosAtJxIu2ThiQHWXIvXILn4rPxCQ3Z5K28beQa/+FGuMmagNJ+jg+AJcAC/9loHbhUGshWeqmYATaClHFH42YRPwYtBS7y2KJXWeR8hYzZpnjA00z2rT4Hyye/46dHu+gQttN6iyH1JKb+AqeNb/WQpUWL7oe4FAT6LJJL9f0qJ9A2pdPuwLvaYsqpb62Cu2TYLSUJAaxN+S4CMkjq9AZyPIuT2TnGCLiFCzXcYtc25py7Qp4ce3sCNqQ1svBLORBa0XMpkNeerpMM/4lm2b7BRazHJVjQxOvPci0uCjbFs4sLEUFOXzS/0chMd49sBLiNzOvkSlqBqN8YT8jnOIse3eOaqvOinrAq25OxzpJ2WuImvGgrywvK/85fflnYgLW4RzeIChaelteFRegPmp2Bn4BWiv3LtNJ7AsAAAAASUVORK5CYII="

if [ "$plugged" -eq 1 ]; then
    echo " | templateImage=${PLUG_B64}"
else
    echo " "
fi

# =========================================================
# Dropdown
# =========================================================
echo "---"

# xbar disables (greys out) any dropdown line that has no click action and no
# submenu, which washes out the text. Give each info line a harmless no-op click
# action so it renders at full strength. `/usr/bin/true` does nothing.
NOOP="shell=/usr/bin/true terminal=false"

echo "Power | size=13 $NOOP"
if [ -n "$draw_w" ]; then
    if [ "$plugged" -eq 1 ]; then
        echo "Live draw: ${draw_w}W (in) | $NOOP"
    else
        echo "Live draw: ${draw_w}W (battery) | $NOOP"
    fi
fi
if [ "$plugged" -eq 1 ]; then
    [ -n "$charger_w" ]    && echo "Charger: ${charger_w}W | $NOOP"
    [ -n "$charger_name" ] && echo "Adapter: ${charger_name} | size=12 $NOOP"
else
    echo "On battery (no adapter) | size=12 $NOOP"
fi

# Always exit 0. The last statement above is a `[ -n … ] && echo` guard, which
# returns exit 1 when the field is empty (e.g. the adapter name is not yet
# populated in ioreg the instant you plug in). Pin the exit code so xbar does
# not flash "exit status 1".
exit 0
