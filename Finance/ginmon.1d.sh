#!/bin/bash

# Add your Ginmon account via `$ security add-generic-password -a "<username>" -s ginmonbitbar -w "<password>"`
# <bitbar.title>Ginmon daily performance</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jonas Bach</bitbar.author>
# <bitbar.author.github>jbach</bitbar.author.github>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.desc>Shows daily performance of Ginmon funds. Add account via `$ security add-generic-password -a "<username>" -s ginmonbitbar -w "<password>"`.</bitbar.desc>

# get user/pass from keychan
SEC=$(security find-generic-password -s ginmonbitbar -g 2>&1)
USER=$(echo "$SEC" | grep "acct" | cut -d \" -f 4)
PASS=$(echo "$SEC" | grep "password" | cut -d \" -f 2)

if [ -z "$USER" ] || [ -z "$PASS" ]; then
	echo "Account details missing | color=red"
	exit 0
fi

# create temporary file
TMP=$(mktemp)

# login
curl 'https://kundencenter.ginmon.de/login' --silent -L -A 'Mozilla/5.0' --cookie-jar "$TMP" >/dev/null
HTML=$(curl 'https://kundencenter.ginmon.de/login' --silent -L -A 'Mozilla/5.0' --data-urlencode "_username=$USER" --data-urlencode "&_password=$PASS" --cookie "$TMP" | tr -d '\040\011\012\015')

# get number
POSITIVE=$(echo "$HTML" | grep -o 'valuepercentpositive">[[:digit:],]*' | cut -d '>' -f 2)
NEGATIVE=$(echo "$HTML" | grep -o 'valuepercentnegative">[[:digit:],]*' | cut -d '>' -f 2)

# output
if [ ! -z "$POSITIVE" ]; then
	SUM=$(echo "$HTML" | grep -o 'valuecurrency-eurpositive">[\.[:digit:],]*' | cut -d '>' -f 2)
	echo "+$POSITIVE% | color=green"
	echo "---"
	echo "+$SUM€ | color=green href=https://kundencenter.ginmon.de/uebersicht"
	exit 0
fi

if [ ! -z "$NEGATIVE" ]; then
	SUM=$(echo "$HTML" | grep -o 'valuecurrency-eurnegative">[\.[:digit:],]*' | cut -d '>' -f 2)
	echo "-$NEGATIVE% | color=red"
	echo "---"
	echo "-$SUM€ | color=red href=https://kundencenter.ginmon.de/uebersicht"
	exit 0
fi

echo "Offline | color=red"
exit 0