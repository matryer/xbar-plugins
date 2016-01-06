#!/usr/bin/env bash
#
# DNS Switcher
# BitBar plugin
#
# by M Saiqul Haq 


# set your network service
network_service="Wi-FI"
tmp_dir="/tmp"
dns_address_script_prefix="$tmp_dir/bitbar_dns_address_"

switcher_script="bitbar_dns_switcher"
switcher_script="$tmp_dir/$switcher_script"

# add or remove list DNS below, then don't forget to make it enabled. see below
google="8.8.8.8
        8.8.4.4"

level3="209.244.0.3
        209.244.0.4

        4.2.2.1
        4.2.2.2

        4.2.2.3
        4.2.2.4"

opendns="208.67.222.222
        208.67.220.220"

norton="199.85.126.10
        199.85.127.10

        199.85.126.20
        199.85.127.20

        199.85.126.30
        199.85.127.30"

enabled_dns_address=(google level3 opendns norton)

echo "Switch DNS"
echo "---"

for dns_name in "${enabled_dns_address[@]}"
do
  switcher="$tmp_dir/bitbar_dns_switcher_${dns_name}"
  cat <<EOF > $switcher
dns_address='$(eval echo \${$dns_name[@]})'
networksetup -setdnsservers $network_service \$(echo \$dns_address)
EOF
  chmod 700 $switcher

  echo "$dns_name | bash=$switcher | terminal=true"
done

