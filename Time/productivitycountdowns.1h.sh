#!/usr/bin/env bash

# <xbar.title>Productivity Countdowns</xbar.title>
# <xbar.version>v1.1.1</xbar.version>
# <xbar.author>Jacopo Lorenzetti</xbar.author>
# <xbar.author.github>jlorenzetti</xbar.author.github>
# <xbar.desc>This plugin will show the current ISO week number and a few productivity boosting countdowns.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/jlorenzetti/xbar-productivity-countdowns/main/image.png</xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl>https://github.com/jlorenzetti/xbar-productivity-countdowns</xbar.abouturl>
# <xbar.var>string(VAR_DATE_OF_BIRTH=1980-12-31): Your date of birth in ISO 8601 format.</xbar.var>
# <xbar.var>number(VAR_LIFE_EXPECTANCY=80): Your life expectancy in years.</xbar.var>

unix_time=$(date +%s)
day_of_year=$(date +%j)
day_of_week=$(date +%w)
week_number=$(date +%V)
month_number=$((10#$(date +%m)))
quarter=$(((month_number - 1) / 3 + 1))

function format_countdown {
    if [[ $1 -lt 0 ]]; then
        echo No "$3" left
    elif [[ $1 -eq 0 ]]; then
        echo Last "$2"
    else
        echo "$1" "$3"
    fi
}

function workdays_to {
    # Adapted from https://stackoverflow.com/a/60220672/18289443
    days=$((($1 - unix_time) / 86400 - 2))
    weeks=$((days / 7))
    frac=$((days % 7))
    if ((day_of_week == 0)); then
        if ((frac > 0)); then
            add=1
        else
            add=0
        fi
    else
        magic=$((frac + (day_of_week + 6) % 7))
        if ((magic < 6)); then
            add=0
        elif ((magic == 6)); then
            add=1
        else
            add=2
        fi
    fi
    holidays=$((weeks * 2 + add))
    workdays=$((days - holidays))

    format_countdown $workdays 'work day' 'work days'
}

end_of_week_unix_time=$(date -v+sun -v23H -v59M -v59S +%s)
workdays_in_week=$(workdays_to "$end_of_week_unix_time")

next_quarter=$((quarter + 1))
[ $next_quarter -gt 4 ] && next_quarter=1
starting_month_of_next_quarter=$(((next_quarter - 1) * 3 + 1))
start_of_next_quarter_unix_time=$(date -j -f "%m-%d %H:%M:%S" "$starting_month_of_next_quarter-01 00:00:00" +%s)
weeks_in_quarter=$(format_countdown $(((start_of_next_quarter_unix_time - unix_time) / 604800)) week weeks)
workdays_in_quarter=$(workdays_to "$start_of_next_quarter_unix_time")

end_of_year_unix_time=$(date -j -f "%m-%d %H:%M:%S" "12-31 23:59:59" +%s)
weeks_in_year=$(format_countdown $(((end_of_year_unix_time - unix_time) / 604800)) week weeks)
workdays_in_year=$(workdays_to "$end_of_year_unix_time")

expected_end_of_life_year=$((${VAR_DATE_OF_BIRTH:0:4} + VAR_LIFE_EXPECTANCY))
expected_end_of_life_unix_time=$(date -j -f "%Y-%m-%d" "$expected_end_of_life_year-${VAR_DATE_OF_BIRTH:5:5}" +%s)
expected_end_of_life_days=$(format_countdown $(((expected_end_of_life_unix_time - unix_time) / 86400)) day days)
expected_end_of_life_weeks=$(format_countdown $(((expected_end_of_life_unix_time - unix_time) / 604800)) week weeks)
expected_end_of_life_quarters=$(format_countdown $(((expected_end_of_life_unix_time - unix_time) / 7889400)) quarter quarters)

echo W"$week_number"
echo "---"
echo Quarter "$quarter" Week "$week_number" Day "$day_of_year"
echo "---"
echo "$workdays_in_week" in W"$week_number"
echo "$workdays_in_quarter" in Q"$quarter"
echo "$workdays_in_year" in the year
echo "---"
echo "$weeks_in_quarter" in Q"$quarter"
echo "$weeks_in_year" in the year
echo "---"
echo "$expected_end_of_life_quarters" to "${VAR_LIFE_EXPECTANCY}"
echo "$expected_end_of_life_weeks" to "${VAR_LIFE_EXPECTANCY}"
echo "$expected_end_of_life_days" to "${VAR_LIFE_EXPECTANCY}"
