#!/usr/local/bin/bash

# <bitbar.title>Mute Mic</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Vasily Pleshakov</bitbar.author>
# <bitbar.author.github>wasapl</bitbar.author.github>
# <bitbar.desc>Mutes or unmutes microphone.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/FI64Y5c.png</bitbar.image>
# <bitbar.dependencies>BASH</bitbar.dependencies>

SCRIPT_NAME=$(basename "$0")
SCRIPT_DIR=$(dirname "$0")
TEMP_FILE="$TMPDIR/${SCRIPT_NAME%%.*}.tmp"
MENU_LINE="| bash='${SCRIPT_DIR}/${SCRIPT_NAME}' param1=switch terminal=false refresh=true"

# icons Made by Dave Gandy
# https://www.flaticon.com/authors/dave-gandy
ICON_MIC="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAAAAABWESUoAAAACXBIWXMAABYlAAAWJQFJUiTwAAAB2UlEQVQ4jY2Sv04UURTGf/fOTqZwC0BjNhSSlURMNiEhhlhQaMIDYGcCHbwAT0ChHW8guhotTKzstKazJDREN/ivUJYAWVeEzM7c+1nM7M66g9FTTeb8zjnf+e6BQViqG604bm1UsVwQlvqunOS0W7+IMIzvKXGSS7Q3jikBAZuKJUmKtUlQbhB9lMsAp/2o1MIy3ZPPAK/e9EBFoSYK+1WGMBqq/EcUQFn3/3WoABAYjSZMRa4AXLlTls5GGNafVEfqqTbXi61qHa0wq6GYZUWdGgYsGEJDxEhEmDAH/rbLUMp5KqSOfBXhEip4lwOi22WSb+2i+OA7k/zoogwwp5+Yp/OGNEunvO0wz+fTvjsVHurnDNcP1fOS76ld5+apHuQmgaVxrqewcCwveR0vwDOdNwrtAVvSGkw12963m1OwJj0auirDlZaSVSxjjcYYltVErcvD72uZO5Fe1DFgqD+XTubyATkVuLnXU/zafndA7fbdS3y5txO4P2wLmNiKs6P0ih9PDASYYoq/sXxn5iqH77dffrD4kvVhhGVRWsQShYPflcFXAoYzOMMQUwbM/WuJjWdg+Vbkw6+vRm4wYElpcS+plvoqczfF/pFJ0tSBS9PEHO333/43jAztfMWb5IQAAAAASUVORK5CYII="
ICON_MIC_SLASH="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAAAAABWESUoAAAACXBIWXMAABYlAAAWJQFJUiTwAAACJElEQVQ4jW2Sz0tUURTHv+e8OzOUYVmLpEUxGCU2EhFBIirhsoVSbspwUctauC6KaBn9AxESBREyLdq1FFxbWEQRQmWQJpKNOda8H/d+W7z3Zu5IZ3k/33vu/Z7zBZql2HNnKQw/3eqA4j+lKL+jJS0Xy6lC2rig6yNjS9qYHzohEJU2RYD7DEmSbPABApHiLr+HoPSZliTpLKdgRF6+2espFD0RXcYnYVSr5EJXS6HoI5u8KFplw/3tbfnJBSlXrbLBrSHPbyZo48MIsEOwg5uWRxOgj7Qer59DoX2OfXRJzkNuDXvvC6ZnOlGhbfH6YPBkOvco6K5xEv0u55HlRUyx1g0BFBAUBCWIXHlWTDA7ERcUSzCQQiYAoCB0KudIj1NkAMI6KN4vmoxTbAgDZ5HLNn/hKGBcfh8/VnEIm79BQAHq9lcsAE2e4FUNZ/ClLsyjcPNGur/Qki7iWhm9dd7zJmkkkIzT8ecg8Jh/TvijMqpVRtxYplubOQJcIx+2rSqdX22o2F/ZB8XVmEsH/DiJVhmGvI4AEJSfkhunsgcEgFJmJyKj2Ho9/x0Hz450YHl8MbDND4hW2eDqzHYWSoaP9nsfEKT5GEHP7bmVJFmZu3sM6u+68CLNz25RjJKjUJS8rChOMsryEWCAHPDtAcbp20vP6+fnixcOxxoeBy6fLrnCt1n6PcZHgDEmbFbCMb9PADGorNsojhMyiePIrlfyOf8DeVZaaM73AA8AAAAASUVORK5CYII="

CUR_INPUT_VOLUME=$(osascript -e "input volume of (get volume settings)")
if [ "${CUR_INPUT_VOLUME}" -eq 0 ]; then
    CUR_ICON=${ICON_MIC_SLASH}
else
    CUR_ICON=${ICON_MIC}
fi

function switch() {
    if [ "${CUR_INPUT_VOLUME}" -eq 0 ]; then
        if [ -s "${TEMP_FILE}" ]; then
            SET_VOLUME=$(cat "${TEMP_FILE}")
        else
            SET_VOLUME=100
        fi
        CUR_ICON=${ICON_MIC}
    else
        SET_VOLUME=0
        CUR_ICON=${ICON_MIC_SLASH}
        echo "${CUR_INPUT_VOLUME}" > "${TEMP_FILE}"
    fi
    osascript -e "set volume input volume $SET_VOLUME"
}

function menu() {
    echo "$MENU_LINE image=${CUR_ICON}"
}

if [[ "$#" -ge 1 ]];then
    if [[ "$1" == 'switch' ]] ; then
        switch
    fi
fi

menu