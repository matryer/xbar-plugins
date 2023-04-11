#!/usr/bin/env bash

#  <xbar.title>Zoom</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Ruslan Potekhin</xbar.author>
#  <xbar.author.github>dievskiy</xbar.author.github>
#  <xbar.image>https://imgur.com/8ca2kvC.png</xbar.image>
#  <xbar.desc>Plugin to open zoom meetings quickly</xbar.desc>

links=()
names=()

if [[ ! -d "$(pwd)/zoom_data" ]]; then
	mkdir "$(pwd)/zoom_data"
fi

if [[ ! -s "$(pwd)/zoom_data/links.txt" ]]; then
	cat <<EOF > "$(pwd)/zoom_data/links.txt"
# File to parse meetings data from. This file should end with a blank line
# put a name and a link delimited by "|", for link only confno parameter should be changed
# example:
# Weekly Meeting | company.zoom.us/join?confno=XXXXXXXXXXX
EOF
fi


while IFS= read -r line; do
	# skip comments
	if [[ $line != \#* ]]; then
		l="$(cut -d '|' -f 2 <<< "$line")"
		name="$(cut -d '|' -f 1 <<< "$line")"
		links+=("$l")
		names+=("$name")
	fi
done <"$(pwd)/zoom_data/links.txt"

echo "Zoom | color=#666666"
echo "---"
for i in "${!links[@]}"; do 
	name="${names[$i]}"
	trimmed_string=$(echo "${links[$i]}" | xargs) 
	printf '%s\n' "$name | color=black shell=/usr/bin/open param1=zoommtg://$trimmed_string"
done

echo "---"
echo "Configure | color=black shell=/usr/bin/open param1=\"$(pwd)/zoom_data/links.txt\""
