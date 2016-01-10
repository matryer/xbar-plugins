#!/bin/bash
# checks for active turns at http://boiteajeux.net every two minutes
# set baj_login=LOGIN and baj_password=PASSWORD in ~/.bitbar

. ~/.bitbar

url="http://www.boiteajeux.net/gestion.php"
data="pAction=login&password=${baj_password}&username=${baj_login}"

fetch() { curl --cookie-jar - --data "$data" --location --silent "$url"; }
extract() { sed -n '/<title>/{s/.*\([0-9][0-9]*\).*/\1/p;q;}'; }

turns="$(extract < <(fetch))"

if ((turns < 1)); then
  echo "BAJ: 0"
else
  echo "BAJ: $turns | color=red"
fi

echo "---"
echo "Play | href=http://boiteajeux.net"
