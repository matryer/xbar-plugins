#!/bin/bash
# <bitbar.title>BGO</bitbar.title>
# <bitbar.version>v1.0.1</bitbar.version>
# <bitbar.author>Justin Honold</bitbar.author>
# <bitbar.author.github>astrostl</bitbar.author.github>
# <bitbar.desc>Checks for active turns at http://www.boardgaming-online.com/ every two minutes</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/1126471/12794798/82c49950-ca7c-11e5-855b-b1c69a0f9f31.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

bgo_login='LOGIN'
bgo_pass='PASSWORD'

url="http://www.boardgaming-online.com/"
data="&identifiant=${bgo_login}&mot_de_passe=${bgo_pass}"

fetch() { curl --cookie-jar - --data "$data" --location --silent "$url"; }
extract() { sed -n '/^nbp=/{s/nbp=\([0-9]*\);/\1/p;q;}'; }

turns="$(extract < <(fetch))"

if ((turns < 1)); then
  echo "BGO: 0"
else
  echo "BGO: $turns | color=red"
fi

echo "---"
echo "Play | href=http://www.boardgaming-online.com/"
