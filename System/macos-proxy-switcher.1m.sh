#!/usr/bin/env bash

# <xbar.title>MacOS Proxy Switcher</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>glowinthedark</xbar.author>
# <xbar.author.github>glowinthedark</xbar.author.github>
# <xbar.desc>Set http and socks5 proxy settings on MacOS.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/glowinthedark/bitbar-plugins/macos-proxy-switcher/images/mac-proxy-switcher.png.png</xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl>https://github.com/glowinthedark/bitbar-plugins/System/macos-proxy-switcher.1m.sh</xbar.abouturl>

# CONFIGURATION
INTERFACE=Wi-Fi

SOCKS_PROXY_HOST=localhost
SOCKS_PROXY_PORT=1080

HTTP_PROXY_HOST=localhost
HTTP_PROXY_PORT=8080

PAC_PROXY="file://$HOME/pac_file_proxy.pac"

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

if [[ "$1" = "enable_pac_proxy" ]]; then
  networksetup -setautoproxyurl  $INTERFACE $PAC_PROXY
  networksetup -setautoproxystate $INTERFACE on
  exit
fi

if [[ "$1" = "disable_pac_proxy" ]]; then
  # networksetup -setautoproxyurl $INTERFACE ""
  networksetup -setautoproxystate $INTERFACE off
  exit
fi


if [[ "$1" = "edit_this_script" ]]; then
  # use default editor for .sh extension
  # open "$0";
  # explicitly use sublimetext3
  open -b com.sublimetext.3 "$0";
  exit
fi

current_socks5_proxy_status=$(networksetup -getsocksfirewallproxy $INTERFACE | awk 'NR=1{print $2; exit}')
current_http_proxy_status=$(networksetup -getwebproxy $INTERFACE | awk 'NR=1{print $2; exit}')
current_pac_proxy_status=$(networksetup -getautoproxyurl $INTERFACE | grep Enabled | awk 'NR=1{print $2; exit}')

# SOCK5 PROXY
if [[ $current_socks5_proxy_status == "Yes" ]] || [[ $current_http_proxy_status == "Yes" ]] || [[ $current_pac_proxy_status == "Yes" ]] ; then

  if [[ $current_socks5_proxy_status == "Yes" ]]; then
    echo '🇬🇧'
    # networksetup -getsocksfirewallproxy $INTERFACE
  fi

  if [[ $current_http_proxy_status == "Yes" ]]; then
    echo '🌍'
    # networksetup -getwebproxy $INTERFACE
  fi

  if [[ $current_pac_proxy_status == "Yes" ]]; then
    echo '📡'
    # networksetup -getautoproxyurl $INTERFACE
  fi
  echo '---'

else
  echo "🇪🇸"
  echo '---'
fi

echo '---'

if [[ $current_socks5_proxy_status == "Yes" ]]; then
  echo "✅ SOCKS PROXY is ON! Click to stop socks5://$SOCKS_PROXY_HOST:$SOCKS_PROXY_PORT | bash='$0' color=indianred param1=disable_socks5_proxy refresh=true terminal=false"
else
  echo "❌ SOCKS PROXY is OFF! Click to start socks5://$SOCKS_PROXY_HOST:$SOCKS_PROXY_PORT | bash='$0' param1=enable_socks5_proxy refresh=true terminal=false"
fi

if [[ $current_http_proxy_status == "Yes" ]]; then
  echo "✅ HTTP PROXY is ON! Click to stop http://$HTTP_PROXY_HOST:$HTTP_PROXY_PORT | bash='$0' color=indianred param1=disable_http_proxy refresh=true terminal=false"
else
  echo "❌ HTTP PROXY is OFF! Click to start http://$HTTP_PROXY_HOST:$HTTP_PROXY_PORT | bash='$0' param1=enable_http_proxy refresh=true terminal=false"
fi


if [[ $current_pac_proxy_status == "Yes" ]]; then
  echo "✅ PAC PROXY is ON! Click to stop $PAC_PROXY | bash='$0' color=indianred param1=disable_pac_proxy refresh=true terminal=false"
else
  echo "❌ PAC PROXY is OFF! Click to start $PAC_PROXY | bash='$0' param1=enable_pac_proxy refresh=true terminal=false"
fi

echo '---'
echo "✏️ Edit this file | bash='$0' param1="edit_this_script" terminal=false"

echo '---'
echo "🔃 Refresh... | refresh=true"
