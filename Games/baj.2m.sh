#!/bin/bash
# <bitbar.title>BAJ</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Justin Honold</bitbar.author>
# <bitbar.author.github>astrostl</bitbar.author.github>
# <bitbar.desc>Checks for active turns at http://www.boiteajeux.net/ every two minutes</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/1126471/12343875/45b78bbe-bafd-11e5-960f-33b77c5d3e41.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

baj_login='LOGIN'
baj_password='PASSWORD'

url="http://www.boiteajeux.net/gestion.php"
data="pAction=login&password=${baj_password}&username=${baj_login}"

fetch() { curl --cookie-jar - --data "$data" --location --silent "$url"; }
extract() { sed -n '/<title>/{s/.*\[\([0-9]*\)\].*/\1/p;q;}'; }

turns="$(extract < <(fetch))"

if ((turns < 1)); then
  echo "BAJ: 0"
else
  echo "BAJ: $turns | color=red"
fi

echo "---"
echo "Play | href=http://www.boiteajeux.net/"
