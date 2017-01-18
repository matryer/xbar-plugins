#!/usr/bin/env bash
#
# DNS Switcher
# The list of DNS options should be defined on this file
#
# <bitbar.title>DNS Switcher</bitbar.title>
# <bitbar.version>v1.4</bitbar.version>
# <bitbar.author>M Saiqul Haq</bitbar.author>
# <bitbar.author.github>saiqulhaq</bitbar.author.github>
# <bitbar.desc>Switch DNS to your defined DNS options.</bitbar.desc>
# <bitbar.image>http://oi66.tinypic.com/2yplm4h.jpg</bitbar.image>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Network/dnsswitcher.1d.sh</bitbar.abouturl>


# Configuration
# set your network service
network_service="Wi-FI"

# add or remove list of DNS options below, don't forget to make it enabled. see below
# shellcheck disable=2034
google="8.8.8.8
        8.8.4.4
        
        2001:4860:4860::8888
        2001:4860:4860::8844"

# shellcheck disable=2034
level3="209.244.0.3
        209.244.0.4

        4.2.2.1
        4.2.2.2

        4.2.2.3
        4.2.2.4"

# shellcheck disable=2034
opendns="208.67.222.222
        208.67.220.220"

# shellcheck disable=2034
norton="199.85.126.10
        199.85.127.10

        199.85.126.20
        199.85.127.20

        199.85.126.30
        199.85.127.30"

enabled_dns_address=(google level3 opendns norton)
########################


selected_dns="Unknown"

IFS=', ' read -r -a current_dns_address <<< "$(networksetup -getdnsservers $network_service | xargs)"

for dns_name in "${enabled_dns_address[@]}"
do
    for current_dns in "${current_dns_address[@]}"
    do
    dns_option="$(eval echo \$"${dns_name}" | xargs)"
        if [[ $dns_option == *"$current_dns"* ]]
        then
            selected_dns="$dns_name"
        fi
    done
done

if [[ $selected_dns == "Unknown" ]]
then
    echo "Unrecognized DNS"
    networksetup -getdnsservers $network_service
else
    echo "$selected_dns"
fi

echo "---"

tmp_dir="/tmp"
for dns_name in "${enabled_dns_address[@]}"
do
  switcher="$tmp_dir/bitbar_dns_switcher_${dns_name}"
  cat <<EOF > "$switcher"
dns_address='$(eval "echo \${${dns_name[*]}}")'
networksetup -setdnsservers $network_service \$(echo \$dns_address)
EOF
  chmod 700 "$switcher"

  echo "$dns_name | bash=$switcher | terminal=true | refresh=true"
done
