#!/bin/bash
#
# <bitbar.title>Sensibo QuickControl</bitbar.title>
# <bitbar.version>v1.2.2</bitbar.version>
# <bitbar.author>Madalin Tache</bitbar.author>
# <bitbar.author.github>niladam</bitbar.author.github>
# <bitbar.dependencies>jq</bitbar.dependencies>
# <bitbar.desc>Provides Sensibo pods (sky v2) status (temperature, humidity, swing and fan) in your menubar allowing you to quickly turn/schedule them on or off</bitbar.desc>
# <bitbar.image>https://i.imgur.com/PyEYqGK.png</bitbar.image>
#
#
# This plugin uses some undocumented Sensibo API functionality
# shamefully borrowed from their web interface.
#
# @Thanks to Omer Enbar (@omere2) for the timer and scheduler (undocumented)
# info :)
#
# This plugin also uses some code from the plugin Network Info bitbar
# plugin by Raymond Kuiper (https://github.com/matryer/bitbar-plugins/blob/master/Network/netinfo.60s.sh)
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

# Function to start the pod for specified minutes.
startForMinutes () {
    POD_DATA=$(curl -4 --connect-timeout 3 -s "https://home.sensibo.com/api/v2/pods/$1/acStates?apiKey=$API_KEY&limit=1" || echo No Data Available)
    POD_AC_STATE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.on')
    POD_TEMPERATURE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.targetTemperature')
    POD_TEMPERATURE_UNIT=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.temperatureUnit')
    POD_MODE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.mode')
    POD_SWING=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.swing')
    POD_FAN=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.fanLevel')
    CURRENTSTATE='{"on":false,"fanLevel":"'${POD_FAN}'","temperatureUnit":"'${POD_TEMPERATURE_UNIT}'","targetTemperature":'${POD_TEMPERATURE}',"mode":"'${POD_MODE}'","swing":"'${POD_SWING}'"}'
    TIMER='{"minutesFromNow": '${2}', "acState": '${CURRENTSTATE}'}'
    curl -X PUT "https://home.sensibo.com/api/v1/pods/$1/timer?apiKey=$API_KEY" -d "$TIMER"
}

# Function to stop the pod after specified minutes.
stopAfterMinutes () {
    POD_DATA=$(curl -4 --connect-timeout 3 -s "https://home.sensibo.com/api/v2/pods/$1/acStates?apiKey=$API_KEY&limit=1" || echo No Data Available)
    POD_AC_STATE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.on')
    POD_TEMPERATURE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.targetTemperature')
    POD_TEMPERATURE_UNIT=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.temperatureUnit')
    POD_MODE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.mode')
    POD_SWING=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.swing')
    POD_FAN=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result[].acState.fanLevel')
    CURRENTSTATE='{"on":false,"fanLevel":"'${POD_FAN}'","temperatureUnit":"'${POD_TEMPERATURE_UNIT}'","targetTemperature":'${POD_TEMPERATURE}',"mode":"'${POD_MODE}'","swing":"'${POD_SWING}'"}'
    TIMER='{"minutesFromNow": '${2}', "acState": '${CURRENTSTATE}'}'
    curl -X PUT "https://home.sensibo.com/api/v1/pods/$1/timer?apiKey=$API_KEY" -d "$TIMER"
}

# Function to turn climate react ON
turnClimateReactON () {
    ENABLE_CLIMATE_REACT='{"enabled": "true"}'
    curl -X PUT "https://home.sensibo.com/api/v2/pods/$1/smartmode/?apiKey=$API_KEY" -d "$ENABLE_CLIMATE_REACT"
}

# Function to turn climate react OFF
turnClimateReactOFF () {
    DISABLE_CLIMATE_REACT='{"enabled": "false"}'
    curl -X PUT "https://home.sensibo.com/api/v2/pods/$1/smartmode/?apiKey=$API_KEY" -d "$DISABLE_CLIMATE_REACT"
}


# Input duration
# @TODO: maybe in a future version.
# askForDuration () {
#     DURATION=$(osascript -e 'set T to text returned of (display dialog "Duration in minutes (default: 5, numbers only)?" buttons {"Cancel", "OK"} default button "OK" default answer "5")')
#     if [ $DURATION -eq $DURATION 2>/dev/null -o $DURATION -eq 0 2>/dev/null ]; then
#         turnon "$2"
#         startForMinutes "$2" "$DURATION"
#         notify "Turned ON $3 for $4 minutes"
#         # notify "Turned ON $3 for $DURATION minutes"
#         exit 0
#     else
#         DURATION=$(osascript -e 'set T to text returned of (display dialog "Duration in minutes (default: 5, numbers only)?" buttons {"Cancel", "OK"} default button "OK" default answer "5")')
#     fi
# }

# If called with turnoff, turn off the specified pod.
if [ "$1" = "turnoff" ]; then
    turnoff "$2"
    notify "Turned OFF $3"
    exit 0
fi

# If called with turnon, turn on the specified pod.
if [ "$1" = "turnon" ]; then
    turnon "$2"
    notify "Turned ON $3"
    exit 0
fi

# If called with timerOn, turn ON the specified pod for specified minutes
if [ "$1" = "timerOn" ]; then
    turnon "$2"
    startForMinutes "$2" "$4"
    notify "Turned ON $3 for $4 minutes"
    exit 0
fi

# If called with timerOff, turn OFF the specified pod for specified minutes
if [ "$1" = "timerOff" ]; then
    stopAfterMinutes "$2" "$4"
    notify "Turning OFF $3 in $4 minutes"
    exit 0
fi

# If called with turnClimateReactON, turn ON Climate React for the specified pod
if [ "$1" = "climatereacton" ]; then
    turnClimateReactON "$2"
    notify "Turned Climate React ON for $3"
    exit 0
fi

# If called with turnClimateReactOFF, turn OFF Climate React for the specified pod
if [ "$1" = "climatereactoff" ]; then
    turnClimateReactOFF "$2"
    notify "Turned Climate React OFF for $3"
    exit 0
fi



# If called with enterDuration, show dialogue and notify.
# @TODO: maybe in a future version.
# if [ "$1" = "enterDuration" ]; then
#     askForDuration
#     exit 0
# fi

# Menubar Title (blue color, with Sensibo Logo)
echo "SENSIBO | color=#3e8fcc image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAl1JREFUOBFlU01rE1EUPfNlMhNrm69qv01FdEKFCCqo0IUIddmNiC6KP0HonxChdlH/ggsXbtxYXBbsQkWhlrrQagalBKNpapxMMp/e+9KZJngXw7x3zz333PPek+ZX30XoC17oMrDvR3jbDaHSunJMRk6T4ISA1IflX4IeRa9Ywsd2IBKbi6exsVhCmlAfaI+JB7r1E3AiRYBXfz3cmTDwfMnEVXMUl87m8WypjPtTGcr5AtNPkijg4m3qslbJ4dHdMiYKGSHtwHZRHE7j4b0ynlzMCwxj40h+FUlCzQsxndehqTL8gAamMNIqvtdtyJQvFXTU3BCMjUMQsKQgjHAupWD9UwMd14eqyIiiCEZKhesHsDseNncPUCRDQ9qPxxAEzOfSziRpe1rr4Ee9LRqEPRE4YWh4vVPHTqOLM0RAQpPTSEZgRk2W8IckblWbgiBGjY7o2PjcxPuWh+OKhENegUkIWIVPY8zSmb2xWvB8mpUIeTQe58rMECy+F7QXy2eGAQI7iDCXUXHjfBY/m47oEH8uzIxgjEZ0ifDIwkOCnnzAouHMbArXzSJajidq2X2O8YKB22O6UEE2JCqEAoaoBNyj+a/NDiOja0hpinCe6/lIeb1g5rDbDcQxxiqSEXw6mnGi/vbLoblDTBUzcAjMwR6wJxblTmmy8EUkOBf/kD8o6woebDVQ3e9i+VYJJ7O6SO/9bmPl5Vc8tmzcHFLB2FiB1P8a2QudXN52fExSp5WFaeH68rqFL1RVMVR6kYMmJgq4FbMyYI6UNOk5z7+oitt4mW5oxVD+K+aafyol6OP5jf33AAAAAElFTkSuQmCC"
echo "---"


# Iterate over the list of pods and generate the values and submenus
for podID in $PODS
do
    # Get current POD state and measurements.
    POD_DATA=$(curl -4 --connect-timeout 3 -s "https://home.sensibo.com/api/v2/pods/$podID?fields=room,acState,measurements,temperatureUnit,smartMode&apiKey=$API_KEY" || echo No Data Available)
    POD_NAME=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.room.name')
    # POD_LOC=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.room.icon')
    POD_AC_STATE=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.on')
    POD_TEMPERATURE_ORIGINAL=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.measurements.temperature')
    POD_TEMPERATURE_UNIT=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.temperatureUnit')
    POD_HUMIDITY=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.measurements.humidity')
    POD_SWING=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.swing')
    POD_FAN=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.acState.fanLevel')
    POD_CLIMATE_REACT_ENABLED=$(echo "$POD_DATA" | /usr/local/bin/jq -r '.result.smartMode.enabled')

    ## If user-defined temperature unit is Fahrenheit, then convert Celsius temperature value to Fahrenheit. If Celsius, keep value obtained from API call as-is.
    if [ "$POD_TEMPERATURE_UNIT" = "F" ]; then
        POD_TEMPERATURE=$(echo "$POD_TEMPERATURE_ORIGINAL * 1.8 + 32" | bc)
    else
        POD_TEMPERATURE=$POD_TEMPERATURE_ORIGINAL
    fi

    # If climate react enabled is set to true then relable as ON, else relable as OFF
    if [ "$POD_CLIMATE_REACT_ENABLED" = "true" ]; then
        POD_CLIMATE_REACT_STATUS="ON"
    else
        POD_CLIMATE_REACT_STATUS="OFF"
    fi

    # Pod is turned on.
    # @TODO: maybe check if theres an error..
    if [ "$POD_AC_STATE" = "true" ]; then
        echo ":white_check_mark: $POD_NAME, $POD_TEMPERATURE °$POD_TEMPERATURE_UNIT 💧 $POD_HUMIDITY% | color=green size=15"
        echo "--SWING: $POD_SWING"
        echo "--FAN: $POD_FAN"
        echo "-- :red_circle: Turn OFF | terminal=false bash='$0' param1=turnoff param2=$podID param3=$POD_NAME refresh=true"
        echo "-- ⏲️ Schedule"
        echo "---- Turn OFF in 5 minutes | terminal=false bash='$0' param1=timerOff param2=$podID param3=$POD_NAME param4=5 refresh=true"
        echo "---- Turn OFF in 10 minutes | terminal=false bash='$0' param1=timerOff param2=$podID param3=$POD_NAME param4=10 refresh=true"
        echo "---- Turn OFF in 15 minutes | terminal=false bash='$0' param1=timerOff param2=$podID param3=$POD_NAME param4=15 refresh=true"
        echo "---- Turn OFF in 30 minutes | terminal=false bash='$0' param1=timerOff param2=$podID param3=$POD_NAME param4=30 refresh=true"
        echo "---- Turn OFF in 60 minutes | terminal=false bash='$0' param1=timerOff param2=$podID param3=$POD_NAME param4=60 refresh=true"
        echo "--Climate React: $POD_CLIMATE_REACT_STATUS"
        echo "---- Turn ON | terminal=false bash='$0' param1=climatereacton param2=$podID param3=$POD_NAME refresh=true"
        echo "---- Turn OFF | terminal=false bash='$0' param1=climatereactoff param2=$podID param3=$POD_NAME refresh=true"

    else
        # Pod is turned off or an error has ocurred.
        echo ":red_circle: $POD_NAME, $POD_TEMPERATURE °$POD_TEMPERATURE_UNIT 💧 $POD_HUMIDITY% | color=red size=15"
        echo "--SWING: $POD_SWING"
        echo "--FAN: $POD_FAN"
        echo "-- :white_check_mark: Turn ON | terminal=false bash='$0' param1=turnon param2=$podID param3=$POD_NAME refresh=true"
        echo "-- ⏲️ Schedule"
        echo "---- Turn ON for 5 minutes | terminal=false bash='$0' param1=timerOn param2=$podID param3=$POD_NAME param4=5 refresh=true"
        echo "---- Turn ON for 10 minutes | terminal=false bash='$0' param1=timerOn param2=$podID param3=$POD_NAME param4=10 refresh=true"
        echo "---- Turn ON for 15 minutes | terminal=false bash='$0' param1=timerOn param2=$podID param3=$POD_NAME param4=15 refresh=true"
        echo "---- Turn ON for 30 minutes | terminal=false bash='$0' param1=timerOn param2=$podID param3=$POD_NAME param4=30 refresh=true"
        echo "---- Turn ON for 60 minutes | terminal=false bash='$0' param1=timerOn param2=$podID param3=$POD_NAME param4=60 refresh=true"
        echo "--Climate React: $POD_CLIMATE_REACT_STATUS"
        echo "---- Turn ON | terminal=false bash='$0' param1=climatereacton param2=$podID param3=$POD_NAME refresh=true"
        echo "---- Turn OFF | terminal=false bash='$0' param1=climatereactoff param2=$podID param3=$POD_NAME refresh=true"
        # Maybe in a future version.
        # echo "---- Turn ON custom time | terminal=false bash='$0' param1=enterDuration param2=$podID param3=$POD_NAME refresh=true"
    fi
done

echo "---"
echo "🔄 Refresh | color=green refresh=true size=15"

# Plugin end.
