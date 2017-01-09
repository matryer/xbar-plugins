#!/bin/bash

# <bitbar.title>bitping/track/graph</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Simon Hudson</bitbar.author>
# <bitbar.author.github>SimonSays13</bitbar.author.github>
# <bitbar.desc>Sends pings to one or more sites, display connectivity status, record results to file (separated daily) and graph the last X minutes performance via the dropdown with no external dependencies</bitbar.desc>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>http://www.provulo.com/</bitbar.abouturl>

# This is a plugin for Bitbar
# https://github.com/matryer/bitbar
#
# Author: (Simon Hudson) simon.hudson@gmail.com
# Based on original bitbar ping by Trung Đinh Quang, Grant Sherrick and Kent Karlsson.
# Includes bitbash code by GaneshV to render bitmap to menu ( https://github.com/ganeshv )
# Theme from http://colorbrewer2.org/

RED_GREEN_THEME=("#d73027" "#fc8d59" "#fee08b" "#d9ef8b" "#91cf60" "#1a9850")

# Configuration

COLORS=(${RED_GREEN_THEME[@]})
MENUFONT=""
FONT=""
MAX_PING=1000
SITES=(8.8.8.8) #Google DNS; SITES=(google.com youtube.com wikipedia.org github.com) using only one site is recommended for graph consistency
GRAPHMINUTES=30
SITE_INDEX=0
PING_TIMES=
bpp=4
rowbytes=$((width * bpp))
pixbytes=$((width * height * bpp))
OLDIFS=$IFS
bmp_header=()
pixels=()

FILE_OUT="$HOME/Documents/PingTest/$(date +%Y%m%d).txt"

#Uncomment if header row required in output file
#if [ ! -f "$FILE_OUT" ]; then
#    echo "date, host, response" >> $FILE_OUT
#fi

# Functions, etc

function colorize {

    if [ "$1" -ge $MAX_PING ]; then

        echo "${COLORS[0]}"

    elif [ "$1" -ge 600 ]; then

        echo "${COLORS[1]}"

    elif [ "$1" -ge 400 ]; then

        echo "${COLORS[2]}"

    elif [ "$1" -ge 200 ]; then

        echo "${COLORS[3]}"

    elif [ "$1" -ge 100 ]; then

        echo "${COLORS[4]}"

    else

        echo "${COLORS[5]}"

    fi

}

hexle32() {

    local num
    printf -v num "%08x" "$1"
    retval="${num:6:2} ${num:4:2} ${num:2:2} ${num:0:2}"

}

errmsg() {

    >&2 echo "$@"

}

make_bmp_header() {

    local headerbytes comp pixoffset filebytes _filebytes _pixoffset
    local _headerbytes _width _height _pixbytes
    bmp_header=()
    headerbytes=40
    comp="00"

    if [ "$bmp_ver" -eq 5 ]; then

        headerbytes=124
        comp="03"

    fi

    pixoffset=$((headerbytes + 14))
    filebytes=$((pixbytes + pixoffset))

    hexle32 $filebytes
    _filebytes=$retval
    hexle32 $pixoffset
    _pixoffset=$retval
    hexle32 $headerbytes
    _headerbytes=$retval
    hexle32 "$width"
    _width=$retval
    hexle32 "$height"
    _height=$retval
    hexle32 $pixbytes
    _pixbytes=$retval

    bmp_header+=(
        42 4d
        $_filebytes
        00 00
        00 00
        $_pixoffset
        $_headerbytes
        $_width
        $_height
        01 00
        20 00
        $comp 00 00 00
        $_pixbytes
        13 0b 00 00
        13 0b 00 00
        00 00 00 00
        00 00 00 00
    )

    if [ "$bmp_ver" -eq 5 ]; then

        bmp_header+=(
            00 00 ff 00
            00 ff 00 00
            ff 00 00 00
            00 00 00 ff
            42 47 52 73
            00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            00 00 00 00
            00 00 00 00
            00 00 00 00
            00 00 00 00
            00 00 00 00
            00 00 00 00
            00 00 00 00
        )

    fi

}

point() {

    local off
    off=$(($2 * rowbytes + $1 * bpp))
    pixels[$off]=${curcol[0]}
    pixels[$((off + 1))]=${curcol[1]}
    pixels[$((off + 2))]=${curcol[2]}
    pixels[$((off + 3))]=${curcol[3]}

}

line() {

    local x1 y1 x2 y2 x y

    if [ "$1" -eq "$3" ]; then

        if [ "$2" -gt "$4" ]; then y1=$4; y2=$2; else y1=$2; y2=$4; fi
        for ((y = y1; y <= y2; y++)); do
            point "$1" $y
        done

    elif [ "$2" -eq "$4" ]; then

        if [ "$1" -gt "$3" ]; then x1=$3; x2=$1; else x1=$1; x2=$3; fi
        for ((x = x1; x <= x2; x++)); do
            point $x "$2"
        done

    else

        errmsg "Only vertical and horizontal lines supported" "$@"

    fi

}

output_bmp() {

    local _bmp=(${bmp_header[@]/#/'\x'})
    _bmp+=(${pixels[@]/#/'\x'})

    local IFS=''
    echo -ne "${_bmp[*]}"
    IFS=$OLDIFS

}

init_bmp() {

    local i
    bmp_ver=${1:-$bmp_ver}
    width=${2:-$width}
    height=${3:-$height}

    rowbytes=$((width * bpp))
    pixbytes=$((width * height * bpp))

    make_bmp_header

    if [ ${#pixels[@]} -ne $pixbytes ]; then

        pixels=()
        for ((i = 0; i < width * height; i++)); do

            pixels+=(${curcol[@]});

        done

    fi
}

#Generate Output

while [ $SITE_INDEX -lt ${#SITES[@]} ]; do

    NEXT_SITE="${SITES[$SITE_INDEX]}"
    NEXT_PING_TIME=$(ping -c 2 -n -q "$NEXT_SITE" 2>/dev/null | awk -F '/' 'END {printf "%.0f\n", $5}')

    if [ "$NEXT_PING_TIME" -eq 0 ]; then

        NEXT_PING_TIME=$MAX_PING

    fi

    if [ -z "$PING_TIMES" ]; then

        PING_TIMES=($NEXT_PING_TIME)

    else

        PING_TIMES=(${PING_TIMES[@]} $NEXT_PING_TIME)

    fi

    SITE_INDEX=$(( SITE_INDEX + 1 ))

done

if [ $NEXT_PING_TIME -ge $MAX_PING ]; then

    MSG="DOWN"

else

    MSG="UP"

fi

echo "$MSG | color=$(colorize $NEXT_PING_TIME) $MENUFONT"
echo "---"

SITE_INDEX=0

while [ $SITE_INDEX -lt ${#SITES[@]} ]; do

    PING_TIME=${PING_TIMES[$SITE_INDEX]}

    echo "$(date '+%d/%m/%Y %H:%M:%S'), ${SITES[$SITE_INDEX]}, $PING_TIME" >> "$FILE_OUT"

    if [ $PING_TIME -eq $MAX_PING ]; then

        PING_TIME="FAIL"

    else

        PING_TIME="$PING_TIME ms | color=$(colorize $PING_TIME) $FONT"

    fi

    echo "${SITES[$SITE_INDEX]}: $PING_TIME"
    SITE_INDEX=$(( SITE_INDEX + 1 ))

done

echo "---"

pixels=()
curcol=(00 00 00 00)
init_bmp 5 $((GRAPHMINUTES*6)) 50
curcol=(00 ff 00 ff)
max_height=50

readings=$(tail -$((GRAPHMINUTES*6)) "$FILE_OUT" | cut -f3 -d ','  | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g')
heights=($readings)

for ((i = 0; i < ${#heights[@]}; i++)); do

    if [ ${#heights[$i]} -gt $max_height ]; then

        line $i 1 $i $max_height

    else

        line $i 1 $i $((1 + ${heights[$i]}))

    fi

done

IMAGE=$(output_bmp | base64)
echo " | image=$IMAGE"

echo "Refresh... | refresh=true"
