#!/bin/bash
# <xbar.title>Multi VPN and Status (OpenConnect)</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.desc>Manage multiple OpenConnect VPNs via xbar</xbar.desc>
# <xbar.author.github>sovsyann</xbar.author.github>

# This was designed to be used on Mac with xbar installed with homebrew
# Keychain Setup on Mac - Entries MUST be created so the script can use the passwords 
# For each host:
# Open Keychain Access → login → Passwords → New Password Item
# Keychain Item Name: https://myhome.vpnserver.com
# Account Name: myusername
# Password: (your VPN password)


#########################################################
# USER CONFIGURATION
#########################################################

VPN_EXECUTABLE="/opt/homebrew/bin/openconnect"
VPN_INTERFACE="utun99"
PUSH_OR_PIN="push"   # push, sms, phone, or token type

# Format: "Name|Host|Group|Username|ServerCert"
# Leave Group or ServerCert empty ("") if not needed.
VPN_PROFILES=(
  "VPN-Home|https://myhome.vpnserver.com||myusername|"
  "VPN-Work|https://myoffice.vpnserver.com|GROUPname|username|pin-sha256:xxxxxxxxxxxxxxxxxKEYxxxxxxxxxx="
  "VPN-Other|https://another.vpnserver.com||myusername"
  
)

# VPN status check and disconnect commands
VPN_CONNECTED="/sbin/ifconfig | grep -A3 $VPN_INTERFACE | grep inet"
VPN_DISCONNECT_CMD="sudo killall -2 openconnect"

#########################################################
# INTERNAL FUNCTIONS
#########################################################

prompt_2fa_method() {
  if [[ "$1" == "push" || "$1" == "sms" || "$1" == "phone" ]]; then
    echo "$1"
  else
    osascript <<EOF
      tell app "System Events"
        text returned of (display dialog "Enter $1 token:" with hidden answer default answer "" buttons {"OK"} default button 1 with title "VPN 2FA")
      end tell
EOF
  fi
}

connect_vpn() {
  NAME=$1
  HOST=$2
  GROUP=$3
  USER=$4
  CERT=$5

  VPN_PASSWORD=$(security find-generic-password -wl "$HOST" -a "$USER" 2>/dev/null)
  if [ -z "$VPN_PASSWORD" ]; then
    osascript -e "display notification \"Missing Keychain entry for $USER@$HOST\" with title \"VPN Connect Error\""
    exit 1
  fi

  AUTHGROUP_ARG=""
  if [ -n "$GROUP" ]; then
    AUTHGROUP_ARG="--authgroup=$GROUP"
  fi

  CERT_ARG=""
  if [ -n "$CERT" ]; then
    CERT_ARG="--servercert=$CERT"
  fi

  # Debug log of command (no password)
  echo "Connecting: $VPN_EXECUTABLE --useragent=AnyConnect $AUTHGROUP_ARG $CERT_ARG -u $USER -i $VPN_INTERFACE $HOST" \
    > /tmp/openconnect-last-command.log

  echo -e "${VPN_PASSWORD}\n$(prompt_2fa_method ${PUSH_OR_PIN})\n" \
    | sudo "$VPN_EXECUTABLE" --useragent=AnyConnect $AUTHGROUP_ARG $CERT_ARG \
        -u "$USER" -i "$VPN_INTERFACE" "$HOST" &>/dev/null &

  # Wait until interface is up
  until eval "$VPN_CONNECTED" >/dev/null 2>&1; do sleep 1; done
  echo "$NAME" > /tmp/openconnect-active-profile
}

disconnect_vpn() {
  sudo killall -2 openconnect 2>/dev/null
  until [ -z "$(eval "$VPN_CONNECTED")" ]; do sleep 1; done
  rm -f /tmp/openconnect-active-profile
}

#########################################################
# MAIN LOGIC
#########################################################

case "$1" in
  connect)
    connect_vpn "$2" "$3" "$4" "$5" "$6"
    ;;
  disconnect)
    disconnect_vpn
    ;;
esac

#########################################################
# XBAR MENU OUTPUT
#########################################################

if [ -n "$(eval "$VPN_CONNECTED")" ]; then
  ACTIVE_VPN=$(cat /tmp/openconnect-active-profile 2>/dev/null)
  echo "VPN: ${ACTIVE_VPN:-Connected} 🔒"
  echo "---"
  echo "Disconnect VPN | bash='$0' param1=disconnect terminal=false refresh=true"
  echo "---"
  echo "Connected to ${ACTIVE_VPN:-unknown}"
else
  echo "VPN ❌"
  echo "---"
  for profile in "${VPN_PROFILES[@]}"; do
    IFS="|" read -r NAME HOST GROUP USER CERT <<< "$profile"
    echo "Connect $NAME | bash='$0' param1=connect param2='$NAME' param3='$HOST' param4='$GROUP' param5='$USER' param6='$CERT' terminal=false refresh=true"
  done
fi
