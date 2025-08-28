#!/usr/bin/env bash

#  <xbar.title>Yet Another DNS Switcher</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>The RatMan</xbar.author>
#  <xbar.author.github>theratman</xbar.author.github>
#  <xbar.desc>Yet another plugin for the xbar app for macOS that allows you to easily change DNS</xbar.desc>
#  <xbar.image>https://github.com/theratman/ya-dns-switcher/screen-preview.png</xbar.image>
#  <xbar.dependencies>shell,awk,route,networksetup,dscacheutil,killall</xbar.dependencies>
#  <xbar.abouturl>https://github.com/theratman/ya-dns-switcher</xbar.abouturl>

set -euo pipefail  # Exit on error, undefined vars, pipe failures

SCRIPT_PATH=$(realpath "$0")
readonly SCRIPT_PATH

# DNS configurations - using indexed array for compatibility with bash 3.2 🤷‍♂️
declare -a dns_names
declare -a dns_servers

dns_names[0]="Cloudflare DNS"
dns_servers[0]="2606:4700:4700::1111 2606:4700:4700::1001 1.1.1.1 1.0.0.1"

dns_names[1]="Google DNS"
dns_servers[1]="2001:4860:4860::8888 2001:4860:4860::8844 8.8.8.8 8.8.4.4"

dns_names[2]="Custom DNS"
dns_servers[2]="192.168.1.1"

readonly dns_names
readonly dns_servers

# Function to display error messages and exit
error_exit() {
    local error_msg="$1"
    local exit_code="${2:-1}"
    echo "Error: ${error_msg}" >&2
    exit "${exit_code}"
}

# Function to get DNS servers by name
get_dns_servers() {
    local provider_name="$1"

    for i in "${!dns_names[@]}"; do
        if [[ "${dns_names[i]}" == "$provider_name" ]]; then
            echo "${dns_servers[i]}"
            return 0
        fi
    done

    error_exit "Unknown DNS provider: ${provider_name}"
}

# Function to get default network service
get_network_service() {
    # Get the default interface name
    local interface
    interface=$(route get default 2>/dev/null | awk '/interface/ {print $2}')

    # Check if we got an interface
    if [[ -z "${interface}" ]]; then
        error_exit "Could not determine default interface"
    fi

    # Get the network service name for the interface (device)
    local network_service
    network_service=$(networksetup -listnetworkserviceorder | \
            awk -v dev="${interface}" '
    BEGIN { capture = 0 }
    /^\([0-9]+\) / {
        if (capture == 1) {
            service_name = ""
            capture = 0
        }
        service_name = $0
        sub(/^\([0-9]+\) /, "", service_name)
        capture = 1
        next
    }
    capture && /Device: / {
        if (index($0, "Device: " dev) > 0) {
            print service_name
            exit 0
        }
        service_name = ""
        capture = 0
    }
    END {
        if (service_name == "") {
            exit 1
        }
    }
    ')

    # Output the result
    if [[ -z "${network_service}" ]]; then
        error_exit "Could not find network service for interface (device) '${interface}'"
    else
        echo "${network_service}"
    fi
}

# Function to get current DNS info for display
get_current_dns_info() {
    local network_service="$1"
    local current_dns

    current_dns=$(networksetup -getdnsservers "${network_service}" 2>/dev/null || true)

    if [[ "${current_dns}" =~ "There aren't any DNS Servers set" ]] || [[ -z "${current_dns}" ]]; then
        echo "No DNS servers configured"
    else
        echo "${current_dns}"
    fi
}

# Generic DNS configuration function
configure_dns() {
    local network_service="$1"
    local dns_provider="$2"

    local servers
    servers=$(get_dns_servers "$dns_provider")

    networksetup -setdnsservers "${network_service}" $servers

    # Clear DNS Cache
    dscacheutil -flushcache
    killall -HUP mDNSResponder 2>/dev/null || killall -HUP mdnsresponder 2>/dev/null || true
}

# Menu creation function
create_menu_action() {
    local label="$1"
    local dns_provider="$2"
    local network_service="$3"

    local servers
    servers=$(get_dns_servers "$dns_provider")
    local formatted_ips="${servers// /, }"

    echo "${label} | refresh=true terminal=false bash='${SCRIPT_PATH}' param1=configure_dns param2='${dns_provider}' param3='${network_service}'"
    echo "-- ${formatted_ips} | refresh=true terminal=false bash='${SCRIPT_PATH}' param1=configure_dns param2='${dns_provider}' param3='${network_service}'"
}

# Display usage information
usage() {
    cat <<EOF
Usage: ${0##*/} [dns_provider] [network_service]
Example: ${0##*/} 'Cloudflare DNS' 'Wi-Fi'

Available DNS providers:
  Cloudflare DNS    Set DNS to Cloudflare (1.1.1.1, 1.0.0.1)
  Google DNS        Set DNS to Google (8.8.8.8, 8.8.4.4)
  Custom DNS        Set DNS to a custom servers

If no arguments are provided, displays a menu of available DNS options.
EOF
}

# Main execution
main() {
    # Handle help request
    if [[ $# -eq 1 && ("$1" == "-h" || "$1" == "--help") ]]; then
        usage
        exit 0
    fi

    # Handle command line arguments if provided
    if [[ $# -ge 1 ]]; then
        local function_name="$1"
        local dns_provider="$2"
        local network_service="${3:-}"

        if [[ -z "${network_service}" ]]; then
            network_service=$(get_network_service)
        fi

        if [[ "${function_name}" == "configure_dns" ]]; then
            configure_dns "${network_service}" "${dns_provider}"
        else
            error_exit "Unknown function '${function_name}'"
        fi
        exit 0
    fi

    # Main menu section
    local network_service
    network_service=$(get_network_service)
    local current_dns_info
    current_dns_info=$(get_current_dns_info "${network_service}")

    echo "🄳🄽🅂 | refresh=true"
    echo "---"
    echo "Interface: ${network_service} | color=blue"
    echo "Current DNS:"
    echo "${current_dns_info}"
    echo "---"

    # Create menu items for each DNS provider
    for i in "${!dns_names[@]}"; do
        local provider="${dns_names[i]}"
        local servers="${dns_servers[i]}"
        local label="${provider}"

        # Check if this provider's servers match the current DNS
        # We'll transform both to a single line for easy comparison
        local current_dns_oneline
        current_dns_oneline=$(echo "${current_dns_info}" | tr '\n' ' ')

        local provider_dns_oneline
        provider_dns_oneline=$(echo "${servers}" | tr '\n' ' ')

        if [[ "${current_dns_oneline}" == *"${provider_dns_oneline}"* ]]; then
            label="✔ ${provider}" # Add a checkmark
        fi

        create_menu_action "$label" "$provider" "${network_service}"
    done
}

# Run main function with all arguments
main "$@"
