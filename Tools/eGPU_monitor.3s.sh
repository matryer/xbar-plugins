#!/bin/bash
# <bitbar.title>eGPU Monitor</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>© 2019 fpsoft</bitbar.author>
# <bitbar.author.github>rastafabisch</bitbar.author.github>
# <bitbar.desc>Get your (e)GPU information on the menu bar! °F/°C</bitbar.desc>

#––––––––––––––––––––––––––––––––––––––––

#
# Get eGPU name – DO NOT EDIT!
#

eGPU=$(system_profiler SPDisplaysDataType | grep Model | grep -o ':.*' | sed  's ..  ' | tail -1)

#––––––––––––––––––––––––––––––––––––––––

#
# Get  other GPU name  – DO NOT EDIT!
#

aGPU=$(system_profiler SPDisplaysDataType | grep Model | grep -o ':.*' | sed  's ..  ' | sed -e '$ d')

#––––––––––––––––––––––––––––––––––––––––

#
# Get GPU stats  – DO NOT EDIT!
#

#usage=$(ioreg -l |grep \"PerformanceStatistics\" | cut -d '{' -f 2 | tr '|' ',' | tr -d '}' | tr ',' '\n'|grep 'Temp\|Fan\|Clock')

temp=$(ioreg -l |grep \"PerformanceStatistics\" | cut -d '{' -f 2 | tr '|' ',' | tr -d '}' | tr ',' '\n'|grep 'Temp' | grep -o '=.*' | sed  's .  ' | tail -1)
tempF=$(expr $temp \* 9 / 5 + 32)

Mem=$(ioreg -l |grep \"PerformanceStatistics\" | cut -d '{' -f 2 | tr '|' ',' | tr -d '}' | tr ',' '\n'|grep 'Clock' | grep -o '=.*' | sed  's .  ' | tail -1)
Core=$(ioreg -l |grep \"PerformanceStatistics\" | cut -d '{' -f 2 | tr '|' ',' | tr -d '}' | tr ',' '\n'|grep 'Clock' | grep -o '=.*' | sed  's .  ' | tail -2 | head -1)

fan=$(ioreg -l |grep \"PerformanceStatistics\" | cut -d '{' -f 2 | tr '|' ',' | tr -d '}' | tr ',' '\n'|grep 'Fan' | grep -o '=.*' | sed  's .  ' | tail -1)
fan2=$(ioreg -l |grep \"PerformanceStatistics\" | cut -d '{' -f 2 | tr '|' ',' | tr -d '}' | tr ',' '\n'|grep 'Fan' | grep -o '=.*' | sed  's .  ' | tail -2 | head -1)


#––––––––––––––––––––––––––––––––––––––––


#
# Colors (Temperature Warning - Temps: °C) – FEEL FREE TO COSTUMISE
#

if [ "$temp" -lt "68" ]; then
    color=green;
elif [ "$temp" -gt "76" ]; then
    color=#DE0000;
else
    color=#FF5700;
fi


#––––––––––––––––––––––––––––––––––––––––

#
# MenuBar header  – FEEL FREE TO CUSTOMISE
#

# °C
#echo "GPU: "$temp"°C | color=$colorT"
echo ""$eGPU" ("$temp"°C) | color=$color"

# °F
#echo "GPU: "$tempF"°F | color=$colorT"
#echo ""$eGPU" ("$tempF"°F) | color=$color"

# Fan RPM
#echo "GPU: "$fan" RPM"
#echo ""$eGPU" ("$fan" RPM)"

#––––––––––––––––––––––––––––––––––––––––

echo "---"

#––––––––––––––––––––––––––––––––––––––––

#
# MenuBar content – FEEL FREE TO CUSTOMISE
#

echo "monitored GPU: ""$eGPU"
echo "---"
echo "Core Clock: "$Core"MHz | color=#7F7F7F"
echo "Memory Clock: "$Mem"MHz | color=#7F7F7F"
echo "Fan Speed: "$fan" RPM ("$fan2"%) | color=#7F7F7F"
echo "Temperatur: "$temp"°C | color=$color" 
#echo "Temperatur: "$tempF"°F | color=$color" 
echo "---"
echo "other GPUs: | color=#7F7F7F"
#echo "----"
#echo "--" "$usage"
echo "$aGPU"