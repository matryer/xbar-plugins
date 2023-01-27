#!/usr/bin/env bash

set -uo pipefail

# <xbar.title>Nagios</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Seren Thompson</xbar.author>
# <xbar.author.github>seren</xbar.author.github>
# <xbar.desc>Nagios status summary with VPN detection. Based on earlier plugin by Rob DeSanno</xbar.desc>
# <xbar.image>https://imgur.com/UvWrm5Y.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/seren/xbar-plugins/</xbar.abouturl>

# Variables become preferences in the app:
#
#  <xbar.var>string(URL="https://nagios.example.com"): Nagios hostname (ex. https://nagios.example.com)</xbar.var>
#  <xbar.var>boolean(CHECKCERT=true): Is the Nagios server is using a valid SSL certificate (as opposed to a self-signed certificate)?</xbar.var>
#  <xbar.var>string(NAME="username"): HTTP auth username</xbar.var>
#  <xbar.var>string(PASSWORD="password"): HTTP auth password</xbar.var>
#  <xbar.var>string(TEST_IP1=""): Optional: IP behind VPN to ping to check VPN connectivity</xbar.var>
#  <xbar.var>string(TEST_IP2=""): Optional: Backup IP behind VPN to ping to check VPN connectivity</xbar.var>

## set variables

TEMP_FILE="$(mktemp)"
TAC="tac.cgi"
STATUS="status.cgi"
DOWN="?hostgroup=all&style=hostdetail&hoststatustypes=4"
CRITICAL="?host=all&style=detail&servicestatustypes=16"
WARNING="?host=all&style=detail&servicestatustypes=4"
UNKNOWN="?host=all&style=detail&servicestatustypes=8"
# OK="?host=all&style=detail&servicestatustypes=2"

if [ "$CHECKCERT" == "true" ]; then
  CURLFLAGS="-s"
else
  CURLFLAGS="-s -k"
fi


pingtest () {
  ping -c1 -W1 "$1" &>/dev/null
}

fn_test_vpn () {
  if [ -n "$TEST_IP1" ] && pingtest "${TEST_IP1}"; then
    return 0
  fi

  if [ -n "$TEST_IP2" ] && pingtest "${TEST_IP2}"; then
    return 0
  fi

  if pingtest 8.8.8.8; then
    # internet ok
    return 1
  else
    # no internet
    return 2
  fi
}

# returns 0 if there's something to report
func_get_nagios_status () {
  # add -k if you are using a self-signed cert
  # shellcheck disable=2086
  curl $CURLFLAGS -u "$NAME:$PASSWORD" "https://$URL/nagios/cgi-bin/$TAC" > $TEMP_FILE

  # Get the number of hosts/services in each category
  down=$(grep "$DOWN" "$TEMP_FILE" | grep Down | cut -d\> -f3 | cut -d' ' -f1)
  critical=$(grep "$CRITICAL" "$TEMP_FILE" | grep Critical | cut -d\> -f3 | cut -d' ' -f1)
  warning=$(grep "$WARNING" "$TEMP_FILE" | grep Warning | cut -d\> -f3 | cut -d' ' -f1)
  unknown=$(grep "$UNKNOWN" "$TEMP_FILE" | grep Unknown | cut -d\> -f3 | cut -d' ' -f1)
  # ok=$(grep "$OK" $TEMP_FILE | grep Ok | cut -d\> -f3 | cut -d' ' -f1)
  for i in down critical warning unknown; do
    if ! [ "${!i}" == 0 ]; then
      bartext+="${!i}${i:0:1},"
    fi
  done

  # strip trailing comma
  bartext="${bartext%%?}"

  # uppercase
  # bartext="${bartext^^}"

  # return 0 if there's something to report
  if [ -n "$bartext" ]; then
    return 0
  else
    return 1
  fi
}


##############
# Main

bartext=''

fn_test_vpn
ret=$?
if [ $ret == 1 ]; then
  echo "no_vpn"
elif [ $ret == 2 ]; then
  echo "no_internet"
else
  if func_get_nagios_status; then
    echo "NAGIOS: ${bartext} | color=red href=https://$URL/nagios/cgi-bin/$TAC"
    echo "---"
    echo "Open all results | color=blue href=https://$URL/nagios/cgi-bin/$TAC"
    [ "$down" = 0 ] || echo "$down DOWN | color=red href=https://$URL/nagios/cgi-bin/$STATUS/$DOWN"
    [ "$critical" = 0 ] || echo "$critical CRITICAL | color=red href=https://$URL/nagios/cgi-bin/$STATUS/$CRITICAL"
    [ "$warning" = 0 ] || echo "$warning WARNING | color=brown href=https://$URL/nagios/cgi-bin/$STATUS/$WARNING"
    [ "$unknown" = 0 ] || echo "$unknown UNKNOWN | color=orange href=https://$URL/nagios/cgi-bin/$STATUS/$UNKNOWN"
  else
    echo "nagios_ok | href=https://$URL/nagios/cgi-bin/$TAC"
    echo "---"
    echo "Open Nagios dashboard | href=https://$URL/nagios/cgi-bin/$TAC"
  fi
fi

# echo "$ok OK | color=green href=https://$URL/nagios/cgi-bin/$STATUS/$OK"

rm -f "$TEMP_FILE"
exit 0
