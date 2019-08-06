#!/bin/sh

# Progress Bar to Beat Procrastination!
#
# by jasonrwang (https://github.com/jasonrwang)
# forked from aurorabbit (https://github.com/aurorabbit) and Mucahit (http://github.com/mucahit)
#
# <bitbar.title>Progress Bar to Beat Procrastination!</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>jasonrwang</bitbar.author>
# <bitbar.author.github>jasonrwang</bitbar.author.github>
# <bitbar.desc>Progress Bar of Year, Month, and (Work) Day, the latter of which helps you keep track of your time as a resource and beat procrastination!</bitbar.desc>
# <bitbar.image>https://user-images.githubusercontent.com/6628281/62536640-f7b71f00-b83d-11e9-9297-a63cc24ef115.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://gist.github.com/jasonrwang/49f76015627cf517fe77eca357401397</bitbar.abouturl>

## !!!
# add this to your bitbar directory
# don't forget to chmod +x

## time parameters
working_start=0900
working_end=2300

## bitbar parameters
# width and characters for the progress bars
# feel free to configure these
width=30
fill_char="█"
empty_char="▁"

# use a monospace font if you want the percentages to be right-aligned
bitbar="size=10 font='Menlo'"
## See Font Book.app's Fixed Width collection for what you can use
## you can also download this font for free and drag it into Font Book.app.
## https://github.com/belluzj/fantasque-sans/releases/latest

## calculations
# all of the calculations are done using unix timestamps from date(1)

# mac uses bsd's date(1)
# whenever we set a date, make sure to add -j so it doesn't change the clock

# we use `date -j %m%d0000 +%s` to get the start timestamp, %Y is implied
# then we use `date -jr $start -v +1y/+1m/+1d +%s` to get the ending timestamp
# then we calculate the percentage with (now - start) / (end - start)

now=$(date +%s)

Y=$(date +%Y)
Y_start=$(date -j 01010000 +%s)
Y_end=$(date -jr "$Y_start" -v +1y +%s)
Y_progress=$(
    echo "($now - $Y_start) * 100 / ($Y_end - $Y_start)" | bc -l
)

m=$(date +%m)
m_start=$(date -j "$(date +%m)"010000 +%s)
m_end=$(date -jr "$m_start" -v +1m +%s)
m_progress=$(
    echo "($now - $m_start) * 100 / ($m_end - $m_start)" | bc -l
)

# If time right now is working_start is less than the starting time but
# midnight, set to "SLP" without %

d=$(date +%d)
d_start=$(date -j $working_start +%s) # starttime of today

# set the end time
d_today=$(date -j "$(date +%m%d)"0000 +%s)
if [ $working_end -eq 0000 ]
then
    d_end=$(date -jr "$d_today" -v +1d +%s) # beginning of next day
else
    d_end=$(date -j "$(date +%m%d)$working_end" +%s) # set to working_end time of today
fi

d_progress=$(
    echo "($now - $d_start) * 100 / ($d_end - $d_start)" | bc -l
)

## output prep
# padding to align progress bar and text
# Y-m-d = 10 + 2 spaces + 2 digits + percent sign = 15
# progress bar width - 15 = padding
padding=$(printf %$((width-6))s "")

# round function
round() { printf %.0f "$1"; }

# progress bar display function
progress() {
    filled=$(round "$(echo "$1 * $width / 100" | bc -l)")
    empty=$((width - filled))
    # repeat the characters using printf
    printf "$fill_char%0.s" $(seq "$filled")
    printf "$empty_char%0.s" $(seq $empty)
}

## output to bitbar
# menu bar line
if [ "$now" -lt "$d_end" ] # tell me to stop if I'm past $working_end
then
    if [ "$now" -lt "$d_start" ] # basically captures post-midnight oil-burning
    then
        echo "😴SLEEP!🛌 | $bitbar size=12 font=SF Compact Text Regular"
    else
        echo "P: $(round "$d_progress")% | $bitbar size=12 font=SF Compact Text Regular"
    fi
else
    echo "🛑STOP!✋ | $bitbar size=12 font=SF Compact Text Regular"
fi
echo ---
# day + progress bar
echo "$Y-$m-$d $padding $(round "$d_progress")%   | $bitbar"
echo "$(progress "$d_progress")                   | $bitbar"
echo ---
# month + progress bar
echo "$Y-$m    $padding $(round "$m_progress")%   | $bitbar"
echo "$(progress "$m_progress")                   | $bitbar"
echo ---
# year + progress bar
echo "$Y       $padding $(round "$Y_progress")%   | $bitbar"
echo "$(progress "$Y_progress")                   | $bitbar"
