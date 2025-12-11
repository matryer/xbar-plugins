#!/usr/bin/env bash
# <xbar.title>Public IP Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Hein Soe</xbar.author>
# <xbar.author.github>hheinsoee</xbar.author.github>
# <xbar.desc>Shows your public IP address, ISP, and location in the menu bar</xbar.desc>
# <xbar.dependencies>bash,curl</xbar.dependencies>
# <xbar.image>https://www.freeiconspng.com/uploads/data-network-icon-image-gallery-5.png</xbar.image>
# <xbar.abouturl>https://github.com/hheinsoee/public-ip-status</xbar.abouturl>

# Cache file for IP info
CACHE_FILE="/tmp/ip-cache"
CACHE_TTL=300  # Cache for 5 minutes

# Function to get public IP info with caching
get_public_ip_info() {
    local current_time=$(date +%s)

    # Check if cache exists and is valid
    if [ -f "$CACHE_FILE" ]; then
        local cache_time=$(stat -f %m "$CACHE_FILE" 2>/dev/null || stat -c %Y "$CACHE_FILE" 2>/dev/null)
        local cache_age=$((current_time - cache_time))

        # Use cache if fresh (< TTL)
        if [ $cache_age -lt $CACHE_TTL ]; then
            cat "$CACHE_FILE"
            return
        fi
    fi

    # Cache miss or expired - fetch new data
    local ip_info=$(curl -s --max-time 3 "https://ipinfo.io/json" 2>/dev/null)

    if [ -n "$ip_info" ]; then
        echo "$ip_info" > "$CACHE_FILE"
        echo "$ip_info"
    fi
}

# Get public IP and location info
IP_INFO=$(get_public_ip_info)

if [ -n "$IP_INFO" ]; then
    # Parse ipinfo.io JSON format using sed
    PUBLIC_IP=$(echo "$IP_INFO" | sed -n 's/.*"ip"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    COUNTRY=$(echo "$IP_INFO" | sed -n 's/.*"country"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    CITY=$(echo "$IP_INFO" | sed -n 's/.*"city"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    REGION=$(echo "$IP_INFO" | sed -n 's/.*"region"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
    ORG=$(echo "$IP_INFO" | sed -n 's/.*"org"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')

    # Extract short ISP name (remove AS number prefix if present)
    ISP_SHORT=$(echo "$ORG" | sed 's/^AS[0-9]* //')
    echo "🌐 $PUBLIC_IP • $ISP_SHORT • $REGION/$COUNTRY"

    echo "---"
    echo "Public IP: $PUBLIC_IP"
    [ -n "$CITY" ] && [ "$CITY" != "null" ] && echo "City: $CITY"
    [ -n "$REGION" ] && [ "$REGION" != "null" ] && echo "Region: $REGION"
    [ -n "$COUNTRY" ] && [ "$COUNTRY" != "null" ] && echo "Country: $COUNTRY"
    [ -n "$ORG" ] && [ "$ORG" != "null" ] && echo "ISP: $ORG"
else
    echo "🌐 IP: Unknown"
    echo "---"
    echo "Unable to fetch IP info"
fi
