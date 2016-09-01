#!/bin/bash
# <bitbar.title>Active Directory Password Expiration Check</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Galen Sprague</bitbar.author>
# <bitbar.author.github>gsprague</bitbar.author.github>
# <bitbar.desc>Shows Active Directory Password Expiration in days</bitbar.desc>
# <bitbar.image>NA</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>NA</bitbar.abouturl>

# Set environment vars
DOMAIN='your domain goes here'
# Set password policy in days
PWPOLICY="90"

# Get logged in user
CURRENTUSER=`ls -l /dev/console | cut -d " " -f4`

# Get data from AD
LASTPWDMS=`dscl /Active\ Directory/$DOMAIN/All\ Domains -read /Users/$CURRENTUSER | grep -i SMBPasswordLastSet | cut -d ' ' -f 2 | tail -1`

# Get the current UNIX date
TODAYUNIX=`date +%s`

# Convert last set value
LASTPWDUNIX=`expr $LASTPWDMS / 10000000 - 11644473600`

# Subtract last set value from current UNIX date
DIFFUNIX=`expr $TODAYUNIX - $LASTPWDUNIX`

# Calculate in days
DIFFDAYS=`expr $DIFFUNIX / 86400`

# Subtract password policy from days
PASSWORDEXPIRATION=`expr $PWPOLICY - $DIFFDAYS`

echo "Password expires in "$PASSWORDEXPIRATION" days"

exit 0
