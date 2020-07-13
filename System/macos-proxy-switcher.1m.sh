#!/usr/bin/env bash

# <bitbar.title>MacOS Proxy Switcher</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>glowinthedark</bitbar.author>
# <bitbar.author.github>glowinthedark</bitbar.author.github>
# <bitbar.desc>Set http and socks5 proxy settings on MacOS.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/glowinthedark/bitbar-plugins/macos-proxy-switcher/images/mac-proxy-switcher.png.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>https://github.com/glowinthedark/bitbar-plugins/System/macos-proxy-switcher.1m.sh</bitbar.abouturl>

# CONFIGURATION
INTERFACE=Wi-Fi

SOCKS_PROXY_HOST=localhost
SOCKS_PROXY_PORT=1080

HTTP_PROXY_HOST=localhost
HTTP_PROXY_PORT=8080
# END CONFIGURATION

if [[ "$1" = "enable_socks5_proxy" ]]; then
  networksetup -setsocksfirewallproxy $INTERFACE $SOCKS_PROXY_HOST $SOCKS_PROXY_PORT
  networksetup -setsocksfirewallproxystate $INTERFACE on
  exit
fi

if [[ "$1" = "disable_socks5_proxy" ]]; then
  networksetup -setsocksfirewallproxystate $INTERFACE off
  exit
fi

if [[ "$1" = "enable_http_proxy" ]]; then
  networksetup -setwebproxy $INTERFACE $HTTP_PROXY_HOST $HTTP_PROXY_PORT
  networksetup -setwebproxystate $INTERFACE on
  exit
fi

if [[ "$1" = "disable_http_proxy" ]]; then
  networksetup -setwebproxystate $INTERFACE off
  exit
fi

if [[ "$1" = "edit_this_script" ]]; then
  # use default editor for .sh extension
  open "$0";
  # explicitly use sublimetext3
  # open -b com.sublimetext.3 "$0";
  exit
fi

current_socks5_proxy_status=$(networksetup -getsocksfirewallproxy $INTERFACE | awk 'NR=1{print $2; exit}')
current_http_proxy_status=$(networksetup -getwebproxy $INTERFACE | awk 'NR=1{print $2; exit}')

# SOCK5 PROXY
if [[ $current_socks5_proxy_status == "Yes" ]]; then
  echo '🌎'
  # echo '---'
  # echo "socks_proxy_enabled = $socks_proxy_enabled"
  echo '---'
  networksetup -getsocksfirewallproxy $INTERFACE
else
  echo "➖"
  echo '---'
fi

# HTTP_PROXY
if [[ $current_http_proxy_status == "Yes" ]]; then
  echo '🌏'
  # echo '---'
  # echo "socks_proxy_enabled = $socks_proxy_enabled"
  echo '---'
  networksetup -getwebproxy $INTERFACE
else
  echo "➖"
  echo '---'
fi

echo '---'

if [[ $current_socks5_proxy_status == "Yes" ]]; then
  echo "🌎 SOCKS PROXY is ON! Click to stop socks5://$SOCKS_PROXY_HOST:$SOCKS_PROXY_PORT | bash='$0' color=indianred param1=disable_socks5_proxy refresh=true terminal=false"
else
  echo "❌ SOCKS PROXY is OFF! Click to start socks5://$SOCKS_PROXY_HOST:$SOCKS_PROXY_PORT | bash='$0' param1=enable_socks5_proxy refresh=true terminal=false"
fi

if [[ $current_http_proxy_status == "Yes" ]]; then
  echo "🌏 HTTP PROXY is ON! Click to stop http://$HTTP_PROXY_HOST:$HTTP_PROXY_PORT | bash='$0' color=indianred param1=disable_http_proxy refresh=true terminal=false"
else
  echo "❌ HTTP PROXY is OFF! Click to start http://$HTTP_PROXY_HOST:$HTTP_PROXY_PORT | bash='$0' param1=enable_http_proxy refresh=true terminal=false"
fi

echo '---'
echo "✏️ Edit this file | bash='$0' param1="edit_this_script" terminal=false"

echo '---'
echo "🔃 Refresh... | refresh=true"
