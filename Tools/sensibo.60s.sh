#!/bin/bash
#
# <bitbar.title>Sensibo PODS</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Madalin Tache</bitbar.author>
# <bitbar.author.github>niladam</bitbar.author.github>
# <bitbar.dependencies>jq</bitbar.dependencies>
# <bitbar.desc>Provides Sensibo pods (sky v2) status in your menubar allowing you to turn them on/off.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/uy3ynSp.png</bitbar.image>
#
#
# If you find any issues, have any suggestions and/or improevements,
# please use the issues via https://github.com/niladam/sensibo-bitbar/issues
#
#
# This plugin uses some undocumented Sensibo API
# functionality shamefully borrowed from their web interface.
#
# This plugin also borrowed some code from the
# Network Info bitbar plugin by Raymond Kuiper
# (https://github.com/matryer/bitbar-plugins/blob/master/Network/netinfo.60s.sh)
#


#####################################################################################
# Your Sensibo API Key - get or generate on from https://home.sensibo.com/me/api    #
#####################################################################################
API_KEY=""

######################################################################################
# STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP !#
#                                                                                    #
#               STOP EDITING HERE UNLESS YOU KNOW WHAT YOU'RE DOING :)               #
#                                                                                    #
# STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP ! STOP !#
######################################################################################

# Checks if the API_KEY has been set.
if [ -z "$API_KEY" ]; then
    echo "SENSIBO | color=#3e8fcc image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAl1JREFUOBFlU01rE1EUPfNlMhNrm69qv01FdEKFCCqo0IUIddmNiC6KP0HonxChdlH/ggsXbtxYXBbsQkWhlrrQagalBKNpapxMMp/e+9KZJngXw7x3zz333PPek+ZX30XoC17oMrDvR3jbDaHSunJMRk6T4ISA1IflX4IeRa9Ywsd2IBKbi6exsVhCmlAfaI+JB7r1E3AiRYBXfz3cmTDwfMnEVXMUl87m8WypjPtTGcr5AtNPkijg4m3qslbJ4dHdMiYKGSHtwHZRHE7j4b0ynlzMCwxj40h+FUlCzQsxndehqTL8gAamMNIqvtdtyJQvFXTU3BCMjUMQsKQgjHAupWD9UwMd14eqyIiiCEZKhesHsDseNncPUCRDQ9qPxxAEzOfSziRpe1rr4Ee9LRqEPRE4YWh4vVPHTqOLM0RAQpPTSEZgRk2W8IckblWbgiBGjY7o2PjcxPuWh+OKhENegUkIWIVPY8zSmb2xWvB8mpUIeTQe58rMECy+F7QXy2eGAQI7iDCXUXHjfBY/m47oEH8uzIxgjEZ0ifDIwkOCnnzAouHMbArXzSJajidq2X2O8YKB22O6UEE2JCqEAoaoBNyj+a/NDiOja0hpinCe6/lIeb1g5rDbDcQxxiqSEXw6mnGi/vbLoblDTBUzcAjMwR6wJxblTmmy8EUkOBf/kD8o6woebDVQ3e9i+VYJJ7O6SO/9bmPl5Vc8tmzcHFLB2FiB1P8a2QudXN52fExSp5WFaeH68rqFL1RVMVR6kYMmJgq4FbMyYI6UNOk5z7+oitt4mW5oxVD+K+aafyol6OP5jf33AAAAAElFTkSuQmCC"
    echo "---"
    echo " NO API KEY PROVIDED | color=red"
    echo "🔄 Refresh | color=green refresh=true size=15"
    exit;
fi

# Generate the PODS URL based on API_KEY.
PODS_URL="https://home.sensibo.com/api/v2/users/me/pods?apiKey=$API_KEY"

# Get a list of available pods.
PODS=$(curl -4 --connect-timeout 3 -s "$PODS_URL" | /usr/local/bin/jq -r '.result[].id' || echo No pods available)

if [[ -z "$PODS" || "$PODS" = "No pods available" ]]; then
    echo "SENSIBO | color=#3e8fcc image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAl1JREFUOBFlU01rE1EUPfNlMhNrm69qv01FdEKFCCqo0IUIddmNiC6KP0HonxChdlH/ggsXbtxYXBbsQkWhlrrQagalBKNpapxMMp/e+9KZJngXw7x3zz333PPek+ZX30XoC17oMrDvR3jbDaHSunJMRk6T4ISA1IflX4IeRa9Ywsd2IBKbi6exsVhCmlAfaI+JB7r1E3AiRYBXfz3cmTDwfMnEVXMUl87m8WypjPtTGcr5AtNPkijg4m3qslbJ4dHdMiYKGSHtwHZRHE7j4b0ynlzMCwxj40h+FUlCzQsxndehqTL8gAamMNIqvtdtyJQvFXTU3BCMjUMQsKQgjHAupWD9UwMd14eqyIiiCEZKhesHsDseNncPUCRDQ9qPxxAEzOfSziRpe1rr4Ee9LRqEPRE4YWh4vVPHTqOLM0RAQpPTSEZgRk2W8IckblWbgiBGjY7o2PjcxPuWh+OKhENegUkIWIVPY8zSmb2xWvB8mpUIeTQe58rMECy+F7QXy2eGAQI7iDCXUXHjfBY/m47oEH8uzIxgjEZ0ifDIwkOCnnzAouHMbArXzSJajidq2X2O8YKB22O6UEE2JCqEAoaoBNyj+a/NDiOja0hpinCe6/lIeb1g5rDbDcQxxiqSEXw6mnGi/vbLoblDTBUzcAjMwR6wJxblTmmy8EUkOBf/kD8o6woebDVQ3e9i+VYJJ7O6SO/9bmPl5Vc8tmzcHFLB2FiB1P8a2QudXN52fExSp5WFaeH68rqFL1RVMVR6kYMmJgq4FbMyYI6UNOk5z7+oitt4mW5oxVD+K+aafyol6OP5jf33AAAAAElFTkSuQmCC"
    echo "---"
    echo " NO PODS DETECTED | color=red"
    echo "🔄 Refresh | color=green refresh=true size=15"
    exit;
fi

# Show a notification
notify () {
    osascript -e "display notification \"$1\" with title \"Sensibo BitBar\""
}

# Function to turn on specific POD
turnon () {
    POD_DATA=$(curl -4 --connect-timeout 3 -s "https://home.sensibo.com/api/v2/pods/$1/acStates?apiKey=$API_KEY&limit=1" || echo No Data Available)
    POD_AC_STATE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.on')
    POD_TEMPERATURE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.targetTemperature')
    POD_TEMPERATURE_UNIT=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.temperatureUnit')
    POD_MODE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.mode')
    POD_SWING=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.swing')
    POD_FAN=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.fanLevel')
    CURRENTSTATE='{"currentAcState":{"on":false,"fanLevel":"'${POD_FAN}'","temperatureUnit":"'${POD_TEMPERATURE_UNIT}'","targetTemperature":'${POD_TEMPERATURE}',"mode":"'${POD_MODE}'","swing":"'${POD_SWING}'"},"newValue":true}'
    curl -X PATCH "https://home.sensibo.com/api/v2/pods/$1/acStates/on?apiKey=$API_KEY" -d "$CURRENTSTATE"
}

# Function to turn off specific POD
turnoff () {
    POD_DATA=$(curl -4 --connect-timeout 3 -s "https://home.sensibo.com/api/v2/pods/$1/acStates?apiKey=$API_KEY&limit=1" || echo No Data Available)
    POD_AC_STATE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.on')
    POD_TEMPERATURE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.targetTemperature')
    POD_TEMPERATURE_UNIT=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.temperatureUnit')
    POD_MODE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.mode')
    POD_SWING=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.swing')
    POD_FAN=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.fanLevel')
    CURRENTSTATE='{"currentAcState":{"on":true,"fanLevel":"'${POD_FAN}'","temperatureUnit":"'${POD_TEMPERATURE_UNIT}'","targetTemperature":'${POD_TEMPERATURE}',"mode":"'${POD_MODE}'","swing":"'${POD_SWING}'"},"newValue":false}'
    curl -X PATCH "https://home.sensibo.com/api/v2/pods/$1/acStates/on?apiKey=$API_KEY" -d "$CURRENTSTATE"
}

# If called with turnoff, turn off the specified pod.
if [ "$1" = "turnoff" ]; then
  turnoff "$2"
  notify "Turned OFF $3"
  exit 0
fi

# If called with turnoff, turn on the specified pod.
if [ "$1" = "turnon" ]; then
  turnon "$2"
  notify "Turned ON $3"
  exit 0
fi

# Menubar Title (blue color, with Sensibo Logo)
echo "SENSIBO | color=#3e8fcc image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAl1JREFUOBFlU01rE1EUPfNlMhNrm69qv01FdEKFCCqo0IUIddmNiC6KP0HonxChdlH/ggsXbtxYXBbsQkWhlrrQagalBKNpapxMMp/e+9KZJngXw7x3zz333PPek+ZX30XoC17oMrDvR3jbDaHSunJMRk6T4ISA1IflX4IeRa9Ywsd2IBKbi6exsVhCmlAfaI+JB7r1E3AiRYBXfz3cmTDwfMnEVXMUl87m8WypjPtTGcr5AtNPkijg4m3qslbJ4dHdMiYKGSHtwHZRHE7j4b0ynlzMCwxj40h+FUlCzQsxndehqTL8gAamMNIqvtdtyJQvFXTU3BCMjUMQsKQgjHAupWD9UwMd14eqyIiiCEZKhesHsDseNncPUCRDQ9qPxxAEzOfSziRpe1rr4Ee9LRqEPRE4YWh4vVPHTqOLM0RAQpPTSEZgRk2W8IckblWbgiBGjY7o2PjcxPuWh+OKhENegUkIWIVPY8zSmb2xWvB8mpUIeTQe58rMECy+F7QXy2eGAQI7iDCXUXHjfBY/m47oEH8uzIxgjEZ0ifDIwkOCnnzAouHMbArXzSJajidq2X2O8YKB22O6UEE2JCqEAoaoBNyj+a/NDiOja0hpinCe6/lIeb1g5rDbDcQxxiqSEXw6mnGi/vbLoblDTBUzcAjMwR6wJxblTmmy8EUkOBf/kD8o6woebDVQ3e9i+VYJJ7O6SO/9bmPl5Vc8tmzcHFLB2FiB1P8a2QudXN52fExSp5WFaeH68rqFL1RVMVR6kYMmJgq4FbMyYI6UNOk5z7+oitt4mW5oxVD+K+aafyol6OP5jf33AAAAAElFTkSuQmCC"
echo "---"


# Iterate over the list of pods and generate the values and submenus
for podID in $PODS
do
    # Get current POD state and measurements.
    POD_DATA=$(curl -4 --connect-timeout 3 -s "https://home.sensibo.com/api/v2/pods/$podID?fields=room,acState,measurements,temperatureUnit&apiKey=$API_KEY" || echo No Data Available)
    POD_NAME=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.room.name')
    # POD_LOC=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.room.icon')
    POD_AC_STATE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.on')
    POD_TEMPERATURE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.measurements.temperature')
    POD_TEMPERATURE_UNIT=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.temperatureUnit')
    POD_HUMIDITY=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.measurements.humidity')
    POD_SWING=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.swing')
    POD_FAN=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.fanLevel')

    # Pod is turned on.
    # @TODO: maybe check if theres an error..
    if [ "$POD_AC_STATE" = "true" ]; then
        echo ":white_check_mark: $POD_NAME, $POD_TEMPERATURE °$POD_TEMPERATURE_UNIT 💧 $POD_HUMIDITY% | color=green size=15"
        echo "--SWING: $POD_SWING"
        echo "--FAN: $POD_FAN"
        echo "--:red_circle: Turn OFF | terminal=false bash='$0' param1=turnoff param2=$podID param3=$POD_NAME refresh=true"
    else
        # Pod is turned off or an error has ocurred.
        echo ":red_circle: $POD_NAME, $POD_TEMPERATURE °$POD_TEMPERATURE_UNIT 💧 $POD_HUMIDITY% | color=red size=15"
        echo "--SWING: $POD_SWING"
        echo "--FAN: $POD_FAN"
        echo "--:white_check_mark: Turn ON | terminal=false bash='$0' param1=turnon param2=$podID param3=$POD_NAME refresh=true"
    fi
done

echo "---"
echo "🔄 Refresh | color=green refresh=true size=15"

# Plugin end.
