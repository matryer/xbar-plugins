#!/bin/bash

# <bitbar.title>External IP (think VPN) country flag emoji</bitbar.title>
# <bitbar.version>v1.5 beta 1</bitbar.version>
# <bitbar.author>Bruce Steedman</bitbar.author>
# <bitbar.author.github>MatzFan</bitbar.author.github>
# <bitbar.desc>Displays country flag emoji - useful if you switch VPN's & need to know remember you 'are' ;-)</bitbar.desc>
#<bitbar.dependencies>OS X 10.11, Sadly Tor breaks it due to rotten CloudFlare Captchas..</bitbar.dependencies>

CODE=$(curl -s http://ipinfo.io | grep country | sed 's/.*"\(.*\)".*/\1/')
COUNTRYANDCODE=$(curl -s http://data.okfn.org/data/core/country-list/r/data.csv|grep $CODE)
COUNTRY=$(echo ${COUNTRYANDCODE%,*}|tr '[:upper:]' '[:lower:]')
TITLE=$(curl -s http://emojipedia.org/flag-for-$COUNTRY/|grep \<title\>)
FLAG=${TITLE%Flag*}
echo ${FLAG##*\>}
