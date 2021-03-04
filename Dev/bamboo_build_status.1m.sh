#!/bin/bash
# <bitbar.title>Bamboo Build Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Nathan Jovin</bitbar.author>
# <bitbar.author.github>njovin</bitbar.author.github>
# <bitbar.desc>Displays Bamboo build status</bitbar.desc>
# <bitbar.image>http://i.imgur.com/RPYrUok.png</bitbar.image>
# <bitbar.dependencies>jq,curl</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Dev/Bamboo/bamboo_build_status.1m.sh</bitbar.abouturl>
#
# Displays the status of a single Bamboo build
# Runs 2 REST calls - one to find out if a build is currently runnning and another to get the last build status if one is not currently running
#
# CONFIGURATION
# All of the below options must be filled in

USERNAME="YOUR_BAMBOO_USERNAME"
PASSWORD="YOUR_BAMBOO_PASSWORD"
SERVER="YOUR_BAMBOO_SERVER"
PLAN="YOUR_BAMBOO_BUILD_PLAN"
JQPATH="jq"

# END CONFIGURATION

PLANRESULT=$(curl -s --user $USERNAME:$PASSWORD  $SERVER/builds/rest/api/latest/plan/$PLAN.json\?os_authType=basic)
BUILDRESULT=$(curl -s --user $USERNAME:$PASSWORD  $SERVER/builds/rest/api/latest/result/$PLAN/latest.json\?os_authType=basic)
ISBUILDING=$(echo "$PLANRESULT" | $JQPATH '.isBuilding')

RELATIVETIME=$(echo "$BUILDRESULT" | $JQPATH '.buildRelativeTime')
BUILDTIME=$(echo "$BUILDRESULT" | $JQPATH '.buildDurationInSeconds')
#TIME=$(echo "$BUILDRESULT" | $JQPATH '.buildRelativeTime')

if [ "$ISBUILDING" = "true" ]
then
    # This is displayed when the build is currently running, feel free to customize
    echo "Building..."
    echo ---
else
    STATE=$(echo "$BUILDRESULT" | $JQPATH '.state')
    # We display the exact state as displayed in the JSON data.
    echo "${STATE//\"/}"
fi

echo ---
# Show how long ago the last build was done
echo "Completed: ${RELATIVETIME//\"/}"
# Show time it took to complete the last build
echo "Build time: $BUILDTIME seconds"
# Show a link to the builds page
echo "Build page | href=$SERVER/builds/browse/$PLAN"
