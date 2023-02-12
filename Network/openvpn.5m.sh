#!/usr/bin/env bash
# <xbar.title>MacOS OpenVPN client & DNS Configuration</xbar.title>
# <xbar.version>v0.88008</xbar.version>
# <xbar.author>glowinthedark</xbar.author>
# <xbar.author.github>glowinthedark</xbar.author.github>
# <xbar.desc>OpenVPN GUI & DNS Manager</xbar.desc>
# <xbar.image>https://telegra.ph/file/7e93fd31b281c9cdcab73.png</xbar.image>
# <xbar.dependencies>openvpn</xbar.dependencies>
# <xbar.abouturl>https://github.com/glowinthedark/bitbar-plugins/blob/master/Network/openvpn.5m.sh</xbar.abouturl>

# *** CONFIGURATION ***

# - INSTALL OPENVPN:

#     brew install openvpn
# OPTIONAL:
#     brew install oath-toolkit (for 2FA token generation)

# NOTE: To avoid having to enter the root password allow
# passwordless sudo for your user on openvpn, killall and pkill:

# 1. Type in terminal: sudo visudo
# 2. Append the following line at the end of the file:

# replace_with_your_username        ALL = NOPASSWD: /usr/local/sbin/openvpn, /usr/bin/killall, /usr/bin/pkill

# 3. Search and replace all __TODO__ lines with your specific commands
# 4. [ON FIRST RUN ONLY] Run the script and grant MacOS accessiblity permissions to the terminal.app, bitbar (or xbar)
#    and to `and to /usr/bin/osascript`

# MacOS Accessibility Permissions
# ===============================

# To enable MacOS system accessibilty permissions run (once) from the terminal:
# osascript -e 'tell application "System Events" to keystroke "t" using command down'


# Under System Preferences select:
#     > Security & Privacy
#     > Privacy tab 
#     > select [Accessibily] in the left panel 
#     > click the 🔒 icon to unlock and add Bitbar.app, Terminal.app and /usr/bin/osascript
#     (if the apps are already enabled in the list, then remove them and re-add)
# In System Settings: allow Terminal full access to Documents (if profiles are in Documents) or full disk access

# START CONFIG
export PATH="$PATH:/usr/local/bin"

OVPN_PROFILE_1=/Users/__TODO__/VPN/openvpn1.ovpn
OVPN_PROFILE_2=/Users/__TODO__/VPN/openvpn2.ovpn
OPENVPN_CMD=/usr/local/sbin/openvpn

# __TODO__the host to ping in order to check if the tunnel is up
PING_TARGET=172.22.0.1

# TODO:  __TODO__: replace with your VPN server's DNS
DNS1=10.0.0.1
DNS2=172.22.0.2
# END CONFIG

################################
# get absolute path to self
function abspath() {
  DIR="${1%/*}"
  (cd "$DIR" && echo "$(pwd -P)/$(basename "$0")")
}
# absolute path to script
script_path="$(abspath "$0")"

#################################
# get MacOS network device name
function get_network_device() {
  scutil --nwi | awk -F': ' '/Network interfaces/ {print $2;exit;}'
}

#################################
# get MacOS network service name: takes network device name as 1st arg
function get_service_name() {
  /usr/sbin/networksetup -listnetworkserviceorder | awk -v DEV="$1" -F': |,' '$0~ DEV  {print $2;exit;}'
}

#################################
# flush DNS cache
function flush_dns_cache() {
  /usr/bin/dscacheutil -flushcache
  sudo /usr/bin/killall -HUP mDNSResponder  
}

NETWORK_DEVICE_NAME="$(get_network_device)"
NETWORK_SERVICE_NAME="$(get_service_name "$NETWORK_DEVICE_NAME")"

#################################
## EDIT THIS SCRIPT 
if [[ "$1" = "edit_this_script" ]]; then
  # open with default editor registered for .sh extension
 open "$0";
  # open with your preferred editor
  # open with vscode
  # /usr/local/bin/code "$0";
  # (via macos app name)
  # open -a 'Sublime Text' "$0"  
  # (via macos bundle name) 
  # open -b com.sublimetext.3 "$0" 
  exit

#################################
# SET INTERNAL DNS SERVERS
elif [[ "$1" = "set_dns_servers_internal" ]]; then
  /usr/sbin/networksetup -setdnsservers "$NETWORK_SERVICE_NAME" $DNS1 $DNS2
  flush_dns_cache 
  exit

#################################
# SET DNS SERVERS
elif [[ "$1" == 'set_dns_servers_custom' ]]; then
  /usr/sbin/networksetup -setdnsservers "$NETWORK_SERVICE_NAME" $2
  flush_dns_cache
  exit

#################################
# START EXPECT SESSION
elif [[ "$1" == 'openvpn_start_expect_session' ]]; then
  OVPN_PROFILE="$2"
  USER="$(echo __TODO__replace_with_command_to_get_user_name)"
  PW=$(echo __TODO__replace_with_command_to_get_user_password)

  # IMPORTANT: IF using 2FA tokens: the OVPN file name MUST contain the sub-strings below!!!
    ## EXAMPLE 1: generate OTP token from secret seed:
    # OTP_CODE=$(oathtool --totp -b DEADBEEF)
    ## EXAMPLE 2: generate OTP token from gpg encrypted seed
    # OTP_CODE=$(gpg --no-verbose --quiet -d secret.gpg | oathtool --totp -b -)

    __TODO__: UNCOMMENT TO USE 2FA TOKENS
#  if [[ $OVPN_PROFILE =~ "openvpn1" ]]; then
#    OTP_CODE=$(echo __TODO__replace_with_command_to_get_vpn1_token)
#  elif [[ $OVPN_PROFILE =~ "openvpn2" ]]; then
#    OTP_CODE=$(echo __TODO__replace_with_command_to_get_vpn2_token)
#  fi

  /usr/bin/expect << EOF
  set timeout -1

  # Detailed log for debugging (uncomment to enable; DANGER!!! THIS WILL INCLUDE PASSWORDS!!!)
  # exp_internal -f /tmp/expect_vpn.log 0

  spawn /usr/bin/sudo $OPENVPN_CMD --allow-pull-fqdn --config $OVPN_PROFILE 
  expect "Auth Username:"
  send -- "$USER\r"
  exec afplay /System/Library/Sounds/Morse.aiff
  expect "Auth Password:"
  send -- "$PW\r"

  ####### OPTIONAL __TODO__ UNCOMMENT TO ENABLE 2FA TOKENS
  # expect "Enter Authenticator Code"
  # send -- "$OTP_CODE\r"

  expect "Initialization Sequence Completed"

  send_user "\n\n✅ OpenVPN Init Complete!\n\n"
  expect_background
  send_user "8️⃣ Setting DNS servers: $DNS1 $DNS2\n\n"
  exec /usr/sbin/networksetup -setdnsservers "$NETWORK_SERVICE_NAME" $DNS1 $DNS2

  #### __TODO__ make sure the glob matches the name of the bitbar plugin file!
  exec open -g "bitbar://refreshPlugin?name=openvpn.*?.sh"
  exec afplay /System/Library/PrivateFrameworks/ToneLibrary.framework/Versions/A/Resources/AlertTones/tweet_sent.caf
  ## __TODO__ uncomment next line to send process to background (requires extra accessiblity permissions)
  # exec "$script_path" send_process_to_background
  expect eof
EOF
  exit
#################################
# DISCONNECT
elif [[ "$1" == 'openvpn_disconnect' ]]; then
  if [[ "$2" == 'force' ]]; then
    sudo /usr/bin/pkill -9 -f "$OPENVPN_CMD"
  else 
    sudo /usr/bin/pkill -f "$OPENVPN_CMD"
  fi
  /usr/sbin/networksetup -setdnsservers "$NETWORK_SERVICE_NAME" empty
  flush_dns_cache
  afplay /System/Library/PrivateFrameworks/ToneLibrary.framework/Versions/A/Resources/AlertTones/ReceivedMessage.caf
  exit

#################################
# SEND CONSOLE TO BACKGROUND
elif [[ "$1" == 'send_process_to_background' ]]; then
  osascript << EOF
tell application "System Events"
  tell application process "Terminal"
    activate
    set frontmost to true
    keystroke "z" using {control down}
    keystroke "bg"
    keystroke return
    keystroke "disown"
    keystroke return
  end tell
end tell
EOF
  exit
#################################
# CONNECT
elif [[ "$1" == 'openvpn_connect_terminal' ]]; then
  ovpn_profile="$2"
  osascript << EOF
    tell application "Terminal"
    if not (exists window 1) then reopen
    activate
    -- __TODO__ uncomment next 2 lines to always open a new tab (requires extra accessiblity permissions)
    tell application "System Events" to keystroke "t" using command down
    delay 0.5
    do script "'$script_path' openvpn_start_expect_session $ovpn_profile" in front window
    end tell
EOF
  exit
fi

#################################
# CHECK VPN (wait 5 sec max until timeout)
DUMMY=$( ping -t 5 -c 1 "$PING_TARGET" 2>&1 >/dev/null)
vpn_status=$?

#################################
# DISPLAY STATUS ICON
if [[ $vpn_status -eq 0 ]] ; then
  # Connected
  echo '⚡️'
  echo '---'

else
  # Not Connected
  echo "📛"
  echo '---'
fi

#################################
# DISPLAY DROPDOWN MENU 
if [[ $vpn_status -eq 0 ]]; then
  label="$(/usr/bin/pgrep -fil $OPENVPN_CMD | awk -F '/' '{print $NF;exit;}')"
  echo "✅ VPN up [${label}] Disconnect! | bash='$0' color=green param1=openvpn_disconnect refresh=true terminal=false"
else
  echo "🔌 Connect OpenVPN1! (Terminal) | bash='$0' color=brown param1=openvpn_connect_terminal param2=$OVPN_PROFILE_1 refresh=false terminal=false"
  echo "🔌 Connect OpenVPN1! (NO Terminal) | bash='$0' color=brown param1=openvpn_start_expect_session param2=$OVPN_PROFILE_1 refresh=false terminal=false"
  echo "🔌 Connect OpenVPN2! | bash='$0' color=brown param1=openvpn_connect_terminal param2=$OVPN_PROFILE_2 refresh=false terminal=false"
  echo "🔌 Connect OpenVPN2! (NO Terminal) | bash='$0' color=brown param1=openvpn_start_expect_session param2=$OVPN_PROFILE_2 refresh=false terminal=false"
fi

echo '---'
echo "7️⃣ Set Custom DNS | bash='$0' param1=set_dns_servers_internal refresh=true terminal=false"
echo "1️⃣ Set Cloudflare DNS | bash='$0' param1=set_dns_servers_custom param2=1.1.1.1 refresh=true terminal=false"
echo "8️⃣ Set Google DNS | bash='$0' param1=set_dns_servers_custom param2=8.8.8.8 refresh=true terminal=false"
echo "↩️ Unset DNS | bash='$0' param1='set_dns_servers_custom' param2=empty refresh=true terminal=false"

echo "❌ Kill all processes and cleanup! | bash='$0' color=darkred param1=openvpn_disconnect refresh=true terminal=false"
echo "💥 FORCE Kill all | bash='$0' color=darkred param1=openvpn_disconnect param2=force refresh=true terminal=false"
echo "🔢 Ping 1.1.1.1 | bash='ping' param1=1.1.1.1 color=darkgreen refresh=false terminal=true"

echo '---'
if [[ $NETWORK_SERVICE_NAME ]] ; then
  echo "Network service: $NETWORK_SERVICE_NAME" \("$NETWORK_DEVICE_NAME"\)
  echo '---'
  echo DNS: $(/usr/sbin/networksetup -getdnsservers "$NETWORK_SERVICE_NAME")

  echo '---'
  echo IP: $(ipconfig getifaddr "$NETWORK_DEVICE_NAME")
else
  echo '🚫  Network is DOWN'
fi
echo '---'
echo "✏️ Edit this file | bash='$0' param1=edit_this_script terminal=false"

echo '---'
echo "🔃 Refresh... | refresh=true"
