#!/bin/bash
# <xbar.title>NetProfiles: Network Profile Manager</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Yusseiin</xbar.author>
# <xbar.author.github>Yusseiin</xbar.author.github>
# <xbar.desc>A network profile manager for macOS</xbar.desc>
# <xbar.dependencies>python3, networksetup</xbar.dependencies>
# <xbar.var>string(VAR_WIFI_INTERFACE="Wi-Fi"): WiFi interface name</xbar.var>
# <xbar.var>string(VAR_ETHERNET_INTERFACE="USB 10/100/1G/2.5G LAN"): Ethernet interface name</xbar.var>
# NetProfiles for macOS - SwiftBar Plugin
# A NetSetMan-like network profile manager

CONFIG_DIR="$HOME/.config/netprofiles"
CONFIG_FILE="$CONFIG_DIR/profiles.json"
CURRENT_WIFI_FILE="$CONFIG_DIR/current_wifi_profile"
CURRENT_ETH_FILE="$CONFIG_DIR/current_eth_profile"
SCRIPT_PATH="$HOME/Documents/SwiftBar/Plugins/NetProfiles.1m.sh"

# Create config directory if it doesn't exist
mkdir -p "$CONFIG_DIR"

# Create default config if it doesn't exist
if [[ ! -f "$CONFIG_FILE" ]]; then
    cat > "$CONFIG_FILE" << 'DEFAULTCONFIG'
{
  "wifi_profiles": [
    {
      "name": "Home DHCP",
      "ip": "dhcp",
      "dns": ["automatic"],
      "proxy": "off"
    },
    {
      "name": "Office WiFi",
      "ip": {
        "address": "192.168.1.100",
        "subnet": "255.255.255.0",
        "router": "192.168.1.1"
      },
      "dns": ["8.8.8.8", "8.8.4.4"],
      "proxy": "off"
    },
    {
      "name": "Privacy DNS",
      "ip": "dhcp",
      "dns": ["1.1.1.1", "1.0.0.1"],
      "proxy": "off"
    }
  ],
  "ethernet_profiles": [
    {
      "name": "Ethernet DHCP",
      "ip": "dhcp",
      "dns": ["automatic"],
      "proxy": "off"
    },
    {
      "name": "Ethernet Static",
      "ip": {
        "address": "10.0.0.50",
        "subnet": "255.255.255.0",
        "router": "10.0.0.1"
      },
      "dns": ["8.8.8.8", "8.8.4.4"],
      "proxy": "off"
    },
    {
      "name": "Ethernet Privacy DNS",
      "ip": "dhcp",
      "dns": ["1.1.1.1", "1.0.0.1"],
      "proxy": "off"
    }
  ],
  "settings": {
    "wifi_interface": "Wi-Fi",
    "ethernet_interface": "USB 10/100/1G/2.5G LAN"
  }
}
DEFAULTCONFIG
    echo "Home DHCP" > "$CURRENT_WIFI_FILE"
    echo "Ethernet DHCP" > "$CURRENT_ETH_FILE"
fi

# ============== HANDLE ACTIONS ==============

if [[ "$1" == "wifi_on" ]]; then
    networksetup -setairportpower en0 on
    osascript -e 'display notification "WiFi enabled" with title "NetProfiles"'
    exit 0
fi

if [[ "$1" == "wifi_off" ]]; then
    networksetup -setairportpower en0 off
    osascript -e 'display notification "WiFi disabled" with title "NetProfiles"'
    exit 0
fi

if [[ "$1" == "apply_wifi" ]]; then
    profile_index="$2"
    
    /usr/bin/python3 << PYTHONSCRIPT
import json
import subprocess
import os

config_file = os.path.expanduser("~/.config/netprofiles/profiles.json")
current_file = os.path.expanduser("~/.config/netprofiles/current_wifi_profile")

with open(config_file) as f:
    data = json.load(f)

profile = data['wifi_profiles'][$profile_index]
interface = data['settings']['wifi_interface']
name = profile['name']

if profile['ip'] == 'dhcp':
    subprocess.run(['networksetup', '-setdhcp', interface], stderr=subprocess.DEVNULL)
else:
    ip_config = profile['ip']
    subprocess.run(['networksetup', '-setmanual', interface, 
                    ip_config['address'], ip_config['subnet'], ip_config['router']], 
                   stderr=subprocess.DEVNULL)

dns = profile['dns']
if dns[0] == 'automatic':
    subprocess.run(['networksetup', '-setdnsservers', interface, 'Empty'], stderr=subprocess.DEVNULL)
else:
    subprocess.run(['networksetup', '-setdnsservers', interface] + dns, stderr=subprocess.DEVNULL)

proxy = profile['proxy']
if proxy == 'off':
    subprocess.run(['networksetup', '-setwebproxystate', interface, 'off'], stderr=subprocess.DEVNULL)
    subprocess.run(['networksetup', '-setsecurewebproxystate', interface, 'off'], stderr=subprocess.DEVNULL)
else:
    if 'http' in proxy:
        subprocess.run(['networksetup', '-setwebproxy', interface, 
                        proxy['http']['host'], str(proxy['http']['port'])], stderr=subprocess.DEVNULL)
        subprocess.run(['networksetup', '-setwebproxystate', interface, 'on'], stderr=subprocess.DEVNULL)
    if 'https' in proxy:
        subprocess.run(['networksetup', '-setsecurewebproxy', interface,
                        proxy['https']['host'], str(proxy['https']['port'])], stderr=subprocess.DEVNULL)
        subprocess.run(['networksetup', '-setsecurewebproxystate', interface, 'on'], stderr=subprocess.DEVNULL)

with open(current_file, 'w') as f:
    f.write(name)

subprocess.run(['osascript', '-e', f'display notification "WiFi: {name}" with title "NetProfiles"'])
PYTHONSCRIPT
    exit 0
fi

if [[ "$1" == "apply_eth" ]]; then
    profile_index="$2"
    
    /usr/bin/python3 << PYTHONSCRIPT
import json
import subprocess
import os

config_file = os.path.expanduser("~/.config/netprofiles/profiles.json")
current_file = os.path.expanduser("~/.config/netprofiles/current_eth_profile")

with open(config_file) as f:
    data = json.load(f)

profile = data['ethernet_profiles'][$profile_index]
interface = data['settings']['ethernet_interface']
name = profile['name']

if profile['ip'] == 'dhcp':
    subprocess.run(['networksetup', '-setdhcp', interface], stderr=subprocess.DEVNULL)
else:
    ip_config = profile['ip']
    subprocess.run(['networksetup', '-setmanual', interface, 
                    ip_config['address'], ip_config['subnet'], ip_config['router']], 
                   stderr=subprocess.DEVNULL)

dns = profile['dns']
if dns[0] == 'automatic':
    subprocess.run(['networksetup', '-setdnsservers', interface, 'Empty'], stderr=subprocess.DEVNULL)
else:
    subprocess.run(['networksetup', '-setdnsservers', interface] + dns, stderr=subprocess.DEVNULL)

proxy = profile['proxy']
if proxy == 'off':
    subprocess.run(['networksetup', '-setwebproxystate', interface, 'off'], stderr=subprocess.DEVNULL)
    subprocess.run(['networksetup', '-setsecurewebproxystate', interface, 'off'], stderr=subprocess.DEVNULL)
else:
    if 'http' in proxy:
        subprocess.run(['networksetup', '-setwebproxy', interface, 
                        proxy['http']['host'], str(proxy['http']['port'])], stderr=subprocess.DEVNULL)
        subprocess.run(['networksetup', '-setwebproxystate', interface, 'on'], stderr=subprocess.DEVNULL)
    if 'https' in proxy:
        subprocess.run(['networksetup', '-setsecurewebproxy', interface,
                        proxy['https']['host'], str(proxy['https']['port'])], stderr=subprocess.DEVNULL)
        subprocess.run(['networksetup', '-setsecurewebproxystate', interface, 'on'], stderr=subprocess.DEVNULL)

with open(current_file, 'w') as f:
    f.write(name)

subprocess.run(['osascript', '-e', f'display notification "Ethernet: {name}" with title "NetProfiles"'])
PYTHONSCRIPT
    exit 0
fi

if [[ "$1" == "edit" ]]; then
    open -a TextEdit "$CONFIG_FILE"
    exit 0
fi

if [[ "$1" == "openprefs" ]]; then
    open "x-apple.systempreferences:com.apple.preference.network"
    exit 0
fi

# ============== MENU BAR OUTPUT ==============

/usr/bin/python3 << 'PYTHONSCRIPT'
import subprocess
import re
import os
import json

config_file = os.path.expanduser("~/.config/netprofiles/profiles.json")
script_path = os.path.expanduser("~/Documents/SwiftBar/Plugins/NetProfiles.1m.sh")

# Load config to get interface names
with open(config_file) as f:
    config = json.load(f)

WIFI_INTERFACE = config['settings']['wifi_interface']
ETHERNET_INTERFACE = config['settings']['ethernet_interface']

def get_wifi_power():
    """Check if WiFi is enabled"""
    try:
        output = subprocess.check_output(
            ['networksetup', '-getairportpower', 'en0'],
            stderr=subprocess.DEVNULL, text=True
        )
        return "On" in output
    except:
        return False

def get_ip_for_interface(iface_name):
    """Get IP address for a network interface by name"""
    try:
        netinfo = subprocess.check_output(
            ['networksetup', '-getinfo', iface_name],
            stderr=subprocess.DEVNULL, text=True
        )
        ip_match = re.search(r'^IP address:\s*(.+)$', netinfo, re.MULTILINE)
        if ip_match:
            ip = ip_match.group(1)
            if ip and not ip.startswith("169.254"):
                return ip
    except:
        pass
    return None

def get_network_info(iface_name):
    """Get full network info for an interface"""
    try:
        netinfo = subprocess.check_output(
            ['networksetup', '-getinfo', iface_name],
            stderr=subprocess.DEVNULL, text=True
        )
        
        ip_match = re.search(r'^IP address:\s*(.+)$', netinfo, re.MULTILINE)
        subnet_match = re.search(r'^Subnet mask:\s*(.+)$', netinfo, re.MULTILINE)
        router_match = re.search(r'^Router:\s*(.+)$', netinfo, re.MULTILINE)
        
        return {
            'ip': ip_match.group(1) if ip_match else None,
            'subnet': subnet_match.group(1) if subnet_match else "N/A",
            'gateway': router_match.group(1) if router_match else "N/A"
        }
    except:
        return {'ip': None, 'subnet': 'N/A', 'gateway': 'N/A'}

def get_ssid():
    """Get current WiFi SSID"""
    try:
        airport_output = subprocess.check_output(
            ['/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport', '-I'],
            stderr=subprocess.DEVNULL, text=True
        )
        ssid_match = re.search(r'^\s*SSID:\s*(.+)$', airport_output, re.MULTILINE)
        return ssid_match.group(1) if ssid_match else None
    except:
        return None

def get_dns(iface_name):
    """Get DNS servers for interface"""
    try:
        dns_output = subprocess.check_output(
            ['networksetup', '-getdnsservers', iface_name],
            stderr=subprocess.DEVNULL, text=True
        ).strip()
        
        if "There aren't any DNS Servers" in dns_output or not dns_output:
            scutil_output = subprocess.check_output(
                ['scutil', '--dns'],
                stderr=subprocess.DEVNULL, text=True
            )
            dns_servers = []
            in_resolver = False
            for line in scutil_output.split('\n'):
                if 'resolver #1' in line:
                    in_resolver = True
                elif in_resolver and 'nameserver' in line:
                    match = re.search(r'nameserver\[\d+\]\s*:\s*(.+)', line)
                    if match:
                        dns_servers.append(match.group(1))
                elif in_resolver and 'resolver #' in line:
                    break
            dns = ', '.join(dns_servers[:2]) if dns_servers else "N/A"
            return dns, "(DHCP)"
        else:
            dns_lines = dns_output.split('\n')
            return ', '.join(dns_lines[:2]), "(Manual)"
    except:
        return "N/A", ""

# Check WiFi power state and connections
wifi_enabled = get_wifi_power()
wifi_ip = get_ip_for_interface(WIFI_INTERFACE) if wifi_enabled else None
ethernet_ip = get_ip_for_interface(ETHERNET_INTERFACE)

# Determine connection type for menu bar icon
if ethernet_ip:
    menubar_icon = "cable.connector"
elif wifi_ip:
    menubar_icon = "wifi"
elif wifi_enabled:
    menubar_icon = "wifi.exclamationmark"  # WiFi on but not connected
else:
    menubar_icon = "wifi.slash"  # WiFi off

# Menu bar icon
print(f"| sfimage={menubar_icon}")
print("---")

# Get current profiles
current_wifi_profile = ""
current_eth_profile = ""
wifi_file = os.path.expanduser("~/.config/netprofiles/current_wifi_profile")
eth_file = os.path.expanduser("~/.config/netprofiles/current_eth_profile")

if os.path.exists(wifi_file):
    with open(wifi_file) as f:
        current_wifi_profile = f.read().strip()
if os.path.exists(eth_file):
    with open(eth_file) as f:
        current_eth_profile = f.read().strip()

# === WiFi Section ===
if wifi_enabled:
    ssid = get_ssid()
    if wifi_ip:
        wifi_info = get_network_info(WIFI_INTERFACE)
        wifi_dns, wifi_dns_source = get_dns(WIFI_INTERFACE)
        if ssid:
            print(f"{ssid} | sfimage=wifi disabled=true")
        print(f"IP: {wifi_info['ip']} | sfimage=number disabled=true")
        print(f"Subnet: {wifi_info['subnet']} | sfimage=square.grid.2x2 disabled=true")
        print(f"Gateway: {wifi_info['gateway']} | sfimage=arrow.triangle.branch disabled=true")
        print(f"DNS: {wifi_dns} {wifi_dns_source} | sfimage=magnifyingglass disabled=true")
        if current_wifi_profile:
            print(f"Profile: {current_wifi_profile} | sfimage=person.crop.circle disabled=true color=#007AFF")
    else:
        print("WiFi: Not connected | sfimage=wifi.exclamationmark disabled=true color=#888888")
    # WiFi toggle - show Turn Off option
    print(f'Turn WiFi Off | sfimage=wifi.slash bash="{script_path}" param1=wifi_off terminal=false refresh=true')
else:
    print("WiFi: Off | sfimage=wifi.slash disabled=true color=#888888")
    # WiFi toggle - show Turn On option
    print(f'Turn WiFi On | sfimage=wifi bash="{script_path}" param1=wifi_on terminal=false refresh=true')

# === Ethernet Section ===
print("---")
if ethernet_ip:
    eth_info = get_network_info(ETHERNET_INTERFACE)
    eth_dns, eth_dns_source = get_dns(ETHERNET_INTERFACE)
    print(f"Ethernet | sfimage=cable.connector disabled=true")
    print(f"IP: {eth_info['ip']} | sfimage=number disabled=true")
    print(f"Subnet: {eth_info['subnet']} | sfimage=square.grid.2x2 disabled=true")
    print(f"Gateway: {eth_info['gateway']} | sfimage=arrow.triangle.branch disabled=true")
    print(f"DNS: {eth_dns} {eth_dns_source} | sfimage=magnifyingglass disabled=true")
    if current_eth_profile:
        print(f"Profile: {current_eth_profile} | sfimage=person.crop.circle disabled=true color=#007AFF")
else:
    print("Ethernet: Not connected | sfimage=cable.connector disabled=true color=#888888")

PYTHONSCRIPT

echo "---"

# WiFi Profile Switcher
echo "WiFi Profiles | sfimage=wifi"

/usr/bin/python3 << 'PYTHONSCRIPT'
import json
import os

config_file = os.path.expanduser("~/.config/netprofiles/profiles.json")
current_file = os.path.expanduser("~/.config/netprofiles/current_wifi_profile")
script_path = os.path.expanduser("~/Documents/SwiftBar/Plugins/NetProfiles.1m.sh")

current_profile = ""
if os.path.exists(current_file):
    with open(current_file) as f:
        current_profile = f.read().strip()

with open(config_file) as f:
    data = json.load(f)

for i, profile in enumerate(data['wifi_profiles']):
    name = profile['name']
    
    if profile['ip'] == 'dhcp':
        ip_desc = 'DHCP'
    else:
        ip_desc = profile['ip'].get('address', 'Static')
    
    is_current = (name == current_profile)
    checkmark = " checked=true" if is_current else ""
    color = " color=#007AFF" if is_current else ""
    
    print(f'--{name} ({ip_desc}) | terminal=false bash="{script_path}" param1=apply_wifi param2={i} refresh=true{checkmark}{color}')

PYTHONSCRIPT

# Ethernet Profile Switcher
echo "Ethernet Profiles | sfimage=cable.connector"

/usr/bin/python3 << 'PYTHONSCRIPT'
import json
import os

config_file = os.path.expanduser("~/.config/netprofiles/profiles.json")
current_file = os.path.expanduser("~/.config/netprofiles/current_eth_profile")
script_path = os.path.expanduser("~/Documents/SwiftBar/Plugins/NetProfiles.1m.sh")

current_profile = ""
if os.path.exists(current_file):
    with open(current_file) as f:
        current_profile = f.read().strip()

with open(config_file) as f:
    data = json.load(f)

for i, profile in enumerate(data['ethernet_profiles']):
    name = profile['name']
    
    if profile['ip'] == 'dhcp':
        ip_desc = 'DHCP'
    else:
        ip_desc = profile['ip'].get('address', 'Static')
    
    is_current = (name == current_profile)
    checkmark = " checked=true" if is_current else ""
    color = " color=#007AFF" if is_current else ""
    
    print(f'--{name} ({ip_desc}) | terminal=false bash="{script_path}" param1=apply_eth param2={i} refresh=true{checkmark}{color}')

PYTHONSCRIPT

echo "---"
echo "Edit Profiles | sfimage=pencil bash='$SCRIPT_PATH' param1=edit terminal=false"
echo "Refresh | sfimage=arrow.clockwise refresh=true"
echo "Network Prefs | sfimage=gear bash='$SCRIPT_PATH' param1=openprefs terminal=false"