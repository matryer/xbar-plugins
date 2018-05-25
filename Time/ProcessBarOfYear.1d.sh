#!/usr/bin/env bash

# Process Bar of Year: Dividing the year into 100 pieces.
#
# by Cnfn (http://github.com/cnfn)
#
# <bitbar.title>Process Bar of Year</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Cnfn</bitbar.author>
# <bitbar.author.github>cnfn</bitbar.author.github>
# <bitbar.desc>Process Bar of Year: Dividing the year into 100 pieces. More info: https://github.com/cnfn/BitBarPlugins/tree/master/ProcessBarOfYear</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/cnfn/grocery/master/images/blog/bitbar_plugin_process_bar_of_year.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/cnfn/BitBarPlugins/tree/master/ProcessBarOfYear</bitbar.abouturl>

get_total_days_of_year() {
	year=$1
	if (( !(year % 4) && ( year % 100 || !(year % 400)  )  ))
	then
		echo 366
	else
		echo 365
	fi
}

days_of_year="$(get_total_days_of_year "$(date +%Y)")"

echo "$(echo "$(date +%j) * 100 / $days_of_year" | bc)""%"
echo "---"
echo -n "$(date +%Y) has completed "
echo -n "$(date +%j | bc)"
echo " days"
