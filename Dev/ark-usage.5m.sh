#!/usr/bin/env bash

# Metadata for xbar
#
#  <xbar.title>Volcengine Ark Usage (火山方舟套餐用量)</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>midasism</xbar.author>
#  <xbar.author.github>midasism</xbar.author.github>
#  <xbar.desc>Shows Volcengine Ark Coding Plan / Agent Plan subscription usage (session, weekly, monthly) with color-coded progress bars in the menu bar. Requires arkcli (npm i -g @volcengine/ark-cli) and a valid login (arkcli auth login).</xbar.desc>
#  <xbar.image>TODO_SCREENSHOT_URL</xbar.image>
#  <xbar.dependencies>bash,jq,bc,arkcli</xbar.dependencies>
#  <xbar.abouturl>https://www.volcengine.com/product/ark</xbar.abouturl>

# <swiftbar.hideAbout>true</swiftbar.hideAbout>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>true</swiftbar.hideLastUpdated>
# <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
# <swiftbar.hideSwiftBar>false</swiftbar.hideSwiftBar>

set -euo pipefail

# Discover Node.js / arkcli in common locations
for p in /usr/local/bin /opt/homebrew/bin "$HOME/.local/bin"; do
    [ -d "$p" ] && export PATH="$p:$PATH"
done
if [ -d "$HOME/.nvm" ]; then
    NVM_NODE=$(ls -d "$HOME/.nvm/versions/node"/*/bin 2>/dev/null | tail -1)
    [ -n "$NVM_NODE" ] && export PATH="$NVM_NODE:$PATH"
fi

# ═══════════════════════════════════════════
# Design Tokens — adapts to light / dark mode
# ═══════════════════════════════════════════

if [ "${XBARDarkMode:-false}" = "true" ]; then
    C_PRIMARY="#F5F5F7"
    C_SECONDARY="#EBEBF5"
    C_TERTIARY="#EBEBF599"
    C_DISABLED="#636366"
    C_BLUE="#0A84FF"
    C_GREEN="#30D158"
    C_ORANGE="#FF9F0A"
    C_RED="#FF453A"
    C_PURPLE="#BF5AF2"

    pct_color() {
        local pct=$1
        if   (( $(echo "$pct > 80" | bc -l) )); then echo "$C_PURPLE"
        elif (( $(echo "$pct > 60" | bc -l) )); then echo "$C_RED"
        elif (( $(echo "$pct > 30" | bc -l) )); then echo "$C_ORANGE"
        else echo "$C_GREEN"
        fi
    }
else
    C_PRIMARY="#1D1D1F"
    C_SECONDARY="#3C3C43"
    C_TERTIARY="#6E6E73"
    C_DISABLED="#AEAEB2"
    C_BLUE="#007AFF"
    C_GREEN="#1D7A34"
    C_ORANGE="#996300"
    C_RED="#CC2F26"
    C_PURPLE="#8B1A1A"

    pct_color() {
        local pct=$1
        if   (( $(echo "$pct > 80" | bc -l) )); then echo "$C_PURPLE"
        elif (( $(echo "$pct > 60" | bc -l) )); then echo "$C_RED"
        elif (( $(echo "$pct > 30" | bc -l) )); then echo "$C_ORANGE"
        else echo "$C_GREEN"
        fi
    }
fi

# ═══ Cache (avoids hammering the API every 5 min) ═══
CACHE_FILE="/tmp/ark-usage-xbar-cache.json"
CACHE_TTL=30

fetch_data() {
    if [ -f "$CACHE_FILE" ]; then
        local now cache_time
        now=$(date +%s)
        if [[ "$OSTYPE" == "darwin"* ]]; then
            cache_time=$(stat -f %m "$CACHE_FILE" 2>/dev/null || echo 0)
        else
            cache_time=$(stat -c %Y "$CACHE_FILE" 2>/dev/null || echo 0)
        fi
        if [ $((now - cache_time)) -lt $CACHE_TTL ]; then
            cat "$CACHE_FILE"
            return 0
        fi
    fi
    local data
    data=$(arkcli usage plan 2>/dev/null) || {
        if [ -f "$CACHE_FILE" ]; then cat "$CACHE_FILE"; return 0; fi
        echo '{"error":"query failed"}'
        return 1
    }
    echo "$data" | tee "$CACHE_FILE"
}

# ═══ Progress bar (Unicode block chars) ═══
bar() {
    local pct=$1 width=16
    local filled=$(echo "$pct * $width / 100" | bc -l | cut -d. -f1)
    local empty=$((width - filled))
    local out=""
    for ((i=0; i<filled; i++)); do out+="▌"; done
    for ((i=0; i<empty; i++)); do out+="·"; done
    printf '%s' "$out"
}

# ═══ Menu-bar dot color (always vivid for both modes) ═══
menu_dot() {
    local pct=$1
    if   (( $(echo "$pct > 80" | bc -l) )); then echo "#AF52DE"
    elif (( $(echo "$pct > 60" | bc -l) )); then echo "#FF453A"
    elif (( $(echo "$pct > 30" | bc -l) )); then echo "#FF9F0A"
    else echo "#30D158"
    fi
}

# ═══ Fetch & parse ═══
DATA=$(fetch_data)

if echo "$DATA" | jq -e '.error' &>/dev/null; then
    echo "● offline | size=11 color=$C_DISABLED"
    echo "---"
    echo "Query failed | color=$C_RED size=13"
    echo "Check network or login | color=$C_TERTIARY size=11"
    echo "---"
    echo "Login… | bash=$(which arkcli) param1=auth param2=login terminal=true refresh=true color=$C_ORANGE"
    exit 0
fi

ITEMS=$(echo "$DATA" | jq -r '.items')
COUNT=$(echo "$ITEMS" | jq 'length')

# Find monthly percentage for menu-bar display
MONTHLY_PCT=0
for i in $(seq 0 $((COUNT - 1))); do
    sub=$(echo "$ITEMS" | jq -r ".[$i].subscribed")
    if [ "$sub" = "true" ]; then
        periods=$(echo "$ITEMS" | jq -r ".[$i].periods")
        pcount=$(echo "$periods" | jq 'length')
        for j in $(seq 0 $((pcount - 1))); do
            lbl=$(echo "$periods" | jq -r ".[$j].label")
            if [ "$lbl" = "monthly" ]; then
                MONTHLY_PCT=$(echo "$periods" | jq -r ".[$j].percent // 0")
            fi
        done
        break
    fi
done

# ═══════════════════════════════════════════
# Menu bar
# ═══════════════════════════════════════════
PCT_INT=$(printf "%.0f" "$MONTHLY_PCT")
DOT_COLOR=$(menu_dot "$MONTHLY_PCT")
echo "● ${PCT_INT}% | size=11 color=$DOT_COLOR"

echo "---"

# ═══════════════════════════════════════════
# Dropdown
# ═══════════════════════════════════════════
HAS_ANY=false
for i in $(seq 0 $((COUNT - 1))); do
    sub=$(echo "$ITEMS" | jq -r ".[$i].subscribed")
    [ "$sub" != "true" ] && continue
    HAS_ANY=true

    product=$(echo "$ITEMS" | jq -r ".[$i].product")
    tier=$(echo "$ITEMS" | jq -r ".[$i].tier // empty")

    case "$product" in
        "coding-plan")      pname="Coding Plan" ;;
        "coding-plan-team") pname="Coding Plan Team" ;;
        "agent-plan")       pname="Agent Plan" ;;
        "agent-plan-team")  pname="Agent Plan Team" ;;
        *)                  pname="$product" ;;
    esac
    [ -n "$tier" ] && pname="$pname · $tier"

    echo "$pname | size=14 color=$C_PRIMARY"

    periods=$(echo "$ITEMS" | jq -r ".[$i].periods")
    pcount=$(echo "$periods" | jq 'length')

    for j in $(seq 0 $((pcount - 1))); do
        label_text=$(echo "$periods" | jq -r ".[$j].label")
        pct=$(echo "$periods" | jq -r ".[$j].percent // 0")
        reset_at=$(echo "$periods" | jq -r ".[$j].reset_at // empty")

        case "$label_text" in
            "5h")      label="5h" ;;
            "session") label="Session" ;;
            "daily")   label="Daily" ;;
            "weekly")  label="Weekly" ;;
            "monthly") label="Monthly" ;;
            *)         label="$label_text" ;;
        esac

        line_color=$(pct_color "$pct")
        bar_str=$(bar "$pct")
        pct_fmt=$(printf "%4.1f" "$pct")

        if [ -n "$reset_at" ] && [ "$reset_at" != "null" ]; then
            if [[ "$OSTYPE" == "darwin"* ]]; then
                reset_fmt=$(date -jf "%Y-%m-%dT%H:%M:%S%z" \
                    "$(echo "$reset_at" | sed 's/\([+-][0-9]\{2\}\):\([0-9]\{2\}\)/\1\2/')" \
                    "+%m/%d %H:%M" 2>/dev/null || echo "$reset_at")
            else
                reset_fmt=$(date -d "$reset_at" "+%m/%d %H:%M" 2>/dev/null || echo "$reset_at")
            fi
            reset_str="↻ $reset_fmt"
        else
            reset_str=""
        fi

        printf "  %-8s  %s  %s%%  %s | color=%s size=12\n" \
            "$label" "$bar_str" "$pct_fmt" "$reset_str" "$line_color"
    done

    echo "---"
done

if [ "$HAS_ANY" = false ]; then
    echo "No active subscription | color=$C_ORANGE size=13"
    echo "Subscribe… | href=https://console.volcengine.com/ark/region:cn-beijing/plans color=$C_BLUE"
    echo "---"
fi

# ═══════════════════════════════════════════
# Footer actions
# ═══════════════════════════════════════════
echo "Updated $(date '+%H:%M') | size=11 color=$C_TERTIARY alternate=true"
echo "Refresh | refresh=true color=$C_BLUE size=12"
echo "Open console… | href=https://console.volcengine.com/ark/region:cn-beijing/subscription/coding-plan color=$C_TERTIARY size=12"
echo "Login… | bash=$(which arkcli) param1=auth param2=login terminal=true refresh=true color=$C_ORANGE size=12"
