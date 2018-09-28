#!/usr/bin/env bash

# Progress Bar of Year, Month and Day: See the big picture.
#
# by Mucahit (http://github.com/mucahit)
#
# <bitbar.title>Progress Bar of Year, Month and Day</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mucahit</bitbar.author>
# <bitbar.author.github>Mucahit</bitbar.author.github>
# <bitbar.desc>Progress Bar of Year, Month and Day: See the big picture.</bitbar.desc>
# <bitbar.image>https://user-images.githubusercontent.com/5108459/43047918-c946f6bc-8de7-11e8-940a-036f44087b92.jpg</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://gist.github.com/mucahit/0bd2ace80ded22328d0c638715a4911b</bitbar.abouturl>

width=20
fill_char="▄"
empty_char=""

bitbar="size=14 color=white font='Avenir'"

now=$(date +%s)

Y_start=$(date -j 01010000 +%s)
Y_end=$(date -jr "$Y_start" -v +1y +%s)
Y_progress=$(
    echo "($now - $Y_start) * 100 / ($Y_end - $Y_start)" | bc -l
)

m_start=$(date -j "$(date +%m)010000" +%s)
m_end=$(date -jr "$m_start" -v +1m +%s)
m_progress=$(
    echo "($now - $m_start) * 100 / ($m_end - $m_start)" | bc -l
)

d_start=$(date -j "$(date +%m%d)0000" +%s)
d_end=$(date -jr "$d_start" -v +1d +%s)
d_progress=$(
    echo "($now - $d_start) * 100 / ($d_end - $d_start)" | bc -l
)

round() { printf %.0f "$1"; }

progress() {
    filled=$(round "$(echo "$1 * $width / 100" | bc -l)")
    empty=$((width - filled))
    # repeat the characters using printf
    printf "$fill_char%0.s" $(seq "$filled")
    printf "$empty_char%0.s" $(seq "$empty")
}

echo "$(round "$Y_progress")%"
echo ---

# day + progress bar
echo "Day: $(round "$d_progress")%   | $bitbar"
echo "$(progress "$d_progress")      | $bitbar"

# month + progress bar
echo " | $bitbar"
echo "Month: $(round "$m_progress")%   | $bitbar"
echo "$(progress "$m_progress")        | $bitbar"

# year + progress bar"
echo " | $bitbar"
echo "Year: $(round "$Y_progress")%   | $bitbar"
echo "$(progress "$Y_progress")       | $bitbar"
