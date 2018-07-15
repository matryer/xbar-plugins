#!/bin/bash

# <bitbar.title>Network Toggler</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>David Shrestha</bitbar.author>
# <bitbar.author.github>davidshr10</bitbar.author.github>
# <bitbar.image>https://i.imgur.com/QRNTmet.png</bitbar.image>
# <bitbar.desc>Provides an easy way to toggle your network connections on and off.</bitbar.desc>
# <bitbar.dependencies>OS X 10.11</bitbar.dependencies>

if [ "$2" == 'toggle_on' ]; then
	networksetup -setnetworkserviceenabled "$1" on
elif [ "$2" == 'toggle_off' ]; then
	networksetup -setnetworkserviceenabled "$1" off
fi

NETWORK_INTERFANCES=$(networksetup -listallnetworkservices |\
awk '/disabled\./,EOF { if(NR>1) print $0 }')

network_icon="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAABFJJREFUWAntl01oXFUYhuf+zF8nE8N08h8wZpIMJNBNDFmoNRTsxnWg7lyq4FLqSgcLRrsoKBXcuNCFEeJOdyoqXYYskyJkEWigJGlJmjDpZO789H0m90zuZCYTKuggeGByzn3P977fd77vnHturFwuZK+tLVihDrTl5eWqtbCw4Ozu7nYkgN7e3qrdgYXXXU5PT3c2ACLpaAb+GwFog9qHh4cWfb14Fwyeh+O208Lx2tpa0djMzMyEk8lk1Ty36s9ytNEi2u2VVrZgLVfF8djc3HRXV1e9cDj8vmVZm9Fo9E2e22WCOWywhQOXBaCFZqsgmgLAcGVlJSxSQUJvlEqlL0V8sVgsvoNAPp9v4hhhM1cqFd+FAxcNtNBsFUST2Pr6ugNhYGCgV06/98WrErrNOJFInJtOMxcORz/3eSFpLPX39/ehibbBTd8UgJnY3t7+QeM0z47jfFgoFO6Njo7GtIpzA2AOG2zFuelrXd7Z2Vnyx01dPQDSs7GxEaZmruvekuU1rG3b/lmpvD0zM3SJZzYZtvRalcsviGFz5Up/Ao5th37iWe2aND9BGx/wT2D95S6Yn593iRwwkYhe1waq+r8t4T11Y3/AztYweH9YPtZgOjY29oJ0toyeyngdA3zhM5fL2bUADHloaCgt411DiMfjr0DQfJfr2p8qrV8NDw9fBhsfH+8Wdkcru8MYjDll7K6wRThg0njV6Kl/hA9wfNYDMNHI4E9jzBHCkCZHHxlcQXwHJkdfBDBOCti3BlMQH4PR/KNcy6rjWPdwjE+yX98DNcvGtPoQ3alZVQ1EjgIlOCmpkAAWOq0zBL9Vzm5hojhTgkdmFaQP3kkJ3EXHse+OjIykwLLZbJL0U4ZsNp0EY44yUS4dvQQYZTR66ndNCZtKwMaAwEYJELbYSODB5gccXG3LTTg+nuqW1gOjx0sJHXyZsrN6++joyJqYmCgfHBzE9vb2/tLKbGX6ddl2HxzsTyvpSxyteLzXyWQy1uTkZEUrcyQY6enpcQOYC5bJXIo8fHjoPXlS+FHcl3GqzNzyPO8bZSk+Nzfn4VOBVOsBAKRSqYreZq5eQr8p6qvivSSBrAJ6KsE/cKbUVrCVo5CeK/wYG2x/f9++f//B00jEvVkuV9/DuTbn7+Vy+W0us8HBwTK24ARwurtAAq2vr+8tPT4GEvmzWCz2ml46hYsuI2y6umJXPa+86Ms9TqfTNxjrDRksW226KYCpqakyNVIWdiKRCEHQLM87rr1azYVzAjf+NXP5/PEHcJhVdm6ghSbajYzg+fJntEJrdnbWg3B8fPyL0s/7QNdx5GtMzIXjmzd0Zk6BY7sJV6v+FS000W4g6KHtZznp/jsfJHwTGEecGC4p8xzsdRIqbb+IICKg+lepn76GWgoFRfliCnLOc244bQPAyAhc9ClmBJ+X07QJg0L/xvj/ADqaAZ0wi3Np60bkMml5ff5T+wDn/Hv+DFoR/VZCBGcPAAAAAElFTkSuQmCC"
echo "|templateImage=$network_icon"
echo "---"

while read -r line; do
    echo "$line"
    if [ "${line:0:1}" == '*' ]; then
    	interface="${line:1}"
    	echo "-- Toggle On | bash='$0' param1='$interface' param2='toggle_on' terminal=false refresh=true color=#3ab24c"
    else
    	echo "-- Toggle Off | bash='$0' param1=$line param2='toggle_off' terminal=false refresh=true color=#d65342"
    fi
done <<< "$NETWORK_INTERFANCES"
