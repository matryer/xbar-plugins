#!/usr/bin/env bash
# <xbar.title>Public IP Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Hein Soe</xbar.author>
# <xbar.author.github>hheinsoee</xbar.author.github>
# <xbar.desc>Shows your public IP address, ISP, and location in the menu bar</xbar.desc>
# <xbar.dependencies>bash,curl</xbar.dependencies>
# <xbar.image>https://ewepkt3zlgn7bph0.public.blob.vercel-storage.com/1765611982304-screenshot2025-12-13at14.01.22-qFAWZqtPhdV13vWikDjFSIqAuHFntM.Png</xbar.image>
# <xbar.abouturl>https://github.com/hheinsoee</xbar.abouturl>
# <xbar.var>string(VAR_CACHE_FILE="/tmp/xbar-public-ip-cache"): Cache file location</xbar.var>

# Cache file for IP info
CACHE_FILE="${VAR_CACHE_FILE:-/tmp/xbar-public-ip-cache}"
NETWORK_STATE_FILE="${CACHE_FILE}.network"

# Handle force refresh parameter
if [ "$1" = "force" ]; then
    rm -f "$CACHE_FILE" "$NETWORK_STATE_FILE"
    exit 0
fi

# Step 1: Check network state locally (NO API CALL)
# Uses routing table to detect gateway + interface
NETWORK_STATE=$(netstat -rn -f inet 2>/dev/null | grep '^default' | head -1 | awk '{print $2 " " $6}')

# No default route = no internet
if [ -z "$NETWORK_STATE" ]; then
    echo "IP: Offline"
    echo "---"
    echo "No internet connection"
    exit 0
fi

# Step 2: Network unchanged + cache exists = use cache (NO API CALL)
IP_INFO=""
if [ -f "$CACHE_FILE" ] && [ -f "$NETWORK_STATE_FILE" ]; then
    CACHED_NETWORK=$(cat "$NETWORK_STATE_FILE")
    if [ "$NETWORK_STATE" = "$CACHED_NETWORK" ]; then
        IP_INFO=$(cat "$CACHE_FILE")
    fi
fi

# Step 3: Network changed or no cache = call API
if [ -z "$IP_INFO" ]; then
    IP_INFO=$(curl -s --max-time 3 "https://ipinfo.io/json" 2>/dev/null)

    # If API succeeds, save to cache
    if [ -n "$IP_INFO" ] && echo "$IP_INFO" | grep -q '"ip"'; then
        echo "$IP_INFO" > "$CACHE_FILE"
        echo "$NETWORK_STATE" > "$NETWORK_STATE_FILE"
    else
        # API failed - try old cache as fallback
        [ -f "$CACHE_FILE" ] && IP_INFO=$(cat "$CACHE_FILE")
    fi
fi

# Step 4: Display info
if [ -n "$IP_INFO" ] && echo "$IP_INFO" | grep -q '"ip"'; then
    PUBLIC_IP=$(echo "$IP_INFO" | sed -n 's/.*"ip"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    COUNTRY=$(echo "$IP_INFO" | sed -n 's/.*"country"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    CITY=$(echo "$IP_INFO" | sed -n 's/.*"city"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    REGION=$(echo "$IP_INFO" | sed -n 's/.*"region"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    ORG=$(echo "$IP_INFO" | sed -n 's/.*"org"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')

    echo "$PUBLIC_IP • $CITY/$REGION/$COUNTRY"
    echo "---"
    echo "Public IP: $PUBLIC_IP"
    [ -n "$CITY" ] && [ "$CITY" != "null" ] && echo "City: $CITY"
    [ -n "$REGION" ] && [ "$REGION" != "null" ] && echo "Region: $REGION"
    [ -n "$COUNTRY" ] && [ "$COUNTRY" != "null" ] && echo "Country: $COUNTRY"
    [ -n "$ORG" ] && [ "$ORG" != "null" ] && echo "ISP: $ORG"
else
    # No info available
    echo "IP: Unknown"
    echo "---"
    echo "Unable to fetch IP info"
fi

echo "---"
echo "🔄 Refresh | bash=\"$0\" param1=force terminal=false refresh=true"
