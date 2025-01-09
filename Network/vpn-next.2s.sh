#!/usr/bin/env bash

# <xbar.title>VPN status/control (via Network Extensions)</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Andrius Vitkauskas</xbar.author>
# <xbar.author.github>anandi108</xbar.author.github>
# <xbar.desc>Displays status of MacOS VPN interfaces (via Network Extensions) with option to connect/disconnect.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/anandi108/xbar_vpn_next/refs/heads/master/img/screenshot.jpg</xbar.image>
# <xbar.abouturl>https://github.com/anandi108/xbar_vpn_next</xbar.abouturl>
# <xbar.dependencies>scutil,bash,date,readlink</xbar.dependencies>

MENU_ICONS="${XBAR_VPN_MENU_ICONS:-default}"   # default|"<icon_off> <icon_on_1> <icon_on_multi> <icon_progress> <dark_icon_off> <dark_icon_on_1> <dark_icon_on_multi> <dark_icon_progress>"
MENU_COLORS="${XBAR_VPN_MENU_COLORS:-default}" # default|"<color_off> <color_on> <color_progress> <dark_color_off> <dark_color_on> <dark_color_progress>"
MENU_COUNT="${XBAR_VPN_MENU_COUNT:-default}"   # default|circled|dice|"<icon0> <icon1> <icon2> ... <icon_more>"
MENU_FONT_SIZE="${XBAR_VPN_MENU_FONT_SIZE:-14}"
MENU_FONT_NAME="${XBAR_VPN_MENU_FONT_NAME:-CourierNew}"

ITEM_ICONS="${XBAR_VPN_ITEM_ICONS:-default}"   # default|"<item_off> <item_on> <item_progress> <dark_item_off> <dark_item_on> <dark_item_progress>"
ITEM_COLORS="${XBAR_VPN_ITEM_COLORS:-default}" # default|"<color_off> <color_on> <color_progress> <dark_color_off> <dark_color_on> <dark_color_progress>"
ITEM_FONT_SIZE="${XBAR_VPN_ITEM_FONT_SIZE:-14}"
ITEM_FONT_NAME="${XBAR_VPN_ITEM_FONT_NAME:-CourierNew}"
ITEM_DISPLAY_DURATION="${XBAR_VPN_DISPLAY_DURATION:-1}"
ITEM_DISPLAY_SECONDS="${XBAR_VPN_DISPLAY_SECONDS:-1}"
ITEM_DISPLAY_TYPE_ICON="${XBAR_VPN_DISPLAY_TYPE_ICON:-1}"

main() {
    parse_config

    local action="$1"
    shift
    case "$action" in
        install|link|symlink)
            install_symlink "$@"
            ;;
        list|list_vpns)
            list_vpns
            ;;
        icon|xbar_icon)
            xbar_icon "$@"
            ;;
        color|xbar_color)
            xbar_color "$@"
            ;;
        start|stop)
            local vpn="$1"
            if [ -z "${vpn// /}" ]; then
                echo "ERROR: VPN profile name is required" >&2
                return 1
            fi
            
            local vpns="$(list_vpns)"
            local regex="^$vpn|.*$"
            if [[ ! "$vpns" =~ $regex ]]; then
                echo "ERROR: VPN profile '$vpn' doesn't exist" >&2
                return 1
            fi

            scutil --nc $action "$vpn"
            ;;
        *)
            xbar_menu "$@"
            ;;
    esac
}

parse_config() {
    define_icons
    define_images

    local default_colors="#555555 #550000 #005500 #999999 #FFFF99 #99FF99"
    #
    MENU_COLORS="${MENU_COLORS/default/}"
    MENU_COLORS=( $( light_dark_subset 3 ${MENU_COLORS:-$default_colors} ) )
    #
    ITEM_COLORS="${ITEM_COLORS/default/}"
    ITEM_COLORS=( $( light_dark_subset 3 ${ITEM_COLORS:-$default_colors} ) )

    MENU_ICONS="${MENU_ICONS/default/}"
    MENU_ICONS=( $(light_dark_subset 4 ${MENU_ICONS:-"$ICON_UNLOCKED $ICON_LOCKED $ICON_LOCKED $ICON_HOURGLASS_FULL"} ) )
    #
    ITEM_ICONS="${ITEM_ICONS/default/}"
    ITEM_ICONS=( $(light_dark_subset 3 ${ITEM_ICONS:-"$ICON_BOXBOX $ICON_LOCKED $ICON_HOURGLASS_COLOR"} ) )
    #
    case "$MENU_COUNT" in
        '')      MENU_COUNT=() ;;
        default) MENU_COUNT=( '' {1..9} $ICON_ELIPSIS ) ;;
        circled) MENU_COUNT=( '' $ICON_CIRCLED_{1..9} $ICON_ELIPSIS ) ;;
        dice)    MENU_COUNT=( '' $ICON_DICE_{1..6} $ICON_ELIPSIS ) ;;
        *)       MENU_COUNT=( $MENU_COUNT ) ;;
    esac
}

light_dark_subset() {
    local size="${1}"
    shift 1
    if [[ "$XBARDarkMode" == "true" && $# -gt 0$size ]]; then
        echo "${@:((size+1)):size}"
    else
        echo "${@:1:size}"
    fi
}

install_symlink() {
    local basepath="$HOME/Library/Application Support/xbar/plugins/vpn-next"

    if [[ "$1" =~ ^-l|--list|list|ls$ ]]; then
        cd "$(dirname "$basepath")"
        ls -l "$(basename "$basepath")"*.sh
        return $r
    fi

    local refresh="${1:-2s}"
    for file in "$basepath"*.sh; do 
        if [ -s "$file" ]; then
            echo "Removing   '${file##*/}'"
            rm -f "$file" 
        fi
    done

    local this="$(readlink -f ${BASH_SOURCE})"
    echo "Installing '$(basename "$basepath").${refresh}.sh' -> '$this'"
    ln -s "$this" "${basepath}.${refresh}.sh"
}

list_vpns() {
    local regex='\* \(([^\)]*)\).*"([^"]*)" *(\[.*\])'
    scutil --nc list | 
    while read line; do
        if [[ "$line" =~ $regex ]]; then 
            echo "${BASH_REMATCH[2]}:${BASH_REMATCH[3]}:${BASH_REMATCH[1]}"
        fi
    done
}

xbar_icon() {
    local count="${1:-0}"
    local progr="${2:-0}"

    if [ 0$count -ge 0${#MENU_COUNT[@]} ]; then
        count=$((${#MENU_COUNT[@]}-1))
    fi

    case "$count" in
        [01])
            local xbar_icon="${MENU_ICONS[$count]}"
            local count_icon="${MENU_COUNT[$count]}"
            ;;
        [0-9]*)
            local xbar_icon="${MENU_ICONS[2]}"
            local count_icon="${MENU_COUNT[$count]}"
            ;;
        *)
            local xbar_icon="${MENU_ICONS[0]}"
            local count_icon="?"
            ;;
    esac

    case "$progr" in
        0|'')   local progr_icon="" ;;
        [0-9]*) local progr_icon="${MENU_ICONS[3]}" ;;
        *)      local progr_icon="" ;;
    esac

    echo "${xbar_icon}${count_icon}${progr_icon}"
}

xbar_color() {
    local count="${1:-0}"
    local progr="${2:-0}"

    local index=0
    if [ 0$progr -ge 1 ]; then
        index=2
    elif [ 0$count -gt 0 ]; then
        index=1
    fi

    echo "${MENU_COLORS[$index]}"
}

format_connected_time() {
    local from="${1:-$(date)}"
    if date --version >/dev/null 2>&1 ; then
        # GNU
        if [ "$ITEM_DISPLAY_DURATION" == "1" ]; then
            local sec="$(($(date +%s) - $(date -d "$from" +%s)))"
            if [ "${ITEM_DISPLAY_SECONDS}" == "1" ]; then
                date -u -d @"$sec" +%H:%M:%S
            else
                date -u -d @"$sec" +%H:%M
            fi
        else
            date -d "$from" '+%Y-%m-%d %T'
        fi
    else
        # BSD
        if [ "$ITEM_DISPLAY_DURATION" == "1" ]; then
            local sec="$(($(date +%s) - $(date -jf'%m/%d/%Y %H:%M:%S' "$from" +%s)))"
            if [ "${ITEM_DISPLAY_SECONDS}" == "1" ]; then
                date -u -r "$sec" +%H:%M:%S
            else
                date -u -r "$sec" +%H:%M
            fi
        else
            date -jf"%m/%d/%Y %T" "$from" '+%Y-%m-%d %H:%M:%S'
        fi
    fi
}

xbar_menu() {
    local items=()
    local count=0
    local progr=0
    local line=''

    list_vpns | {
        while read line; do
            if [[ ! "$line" =~ (.*):\[(.*)\]:(.*) ]]; then
                echo "ERROR: wrong list_vpns() output - '$line'" >&2
                next
            fi
            local name="${BASH_REMATCH[1]}"
            local icon=""
            local color="${ITEM_COLORS[0]}"
            local type="${BASH_REMATCH[2]}"
            local status="${BASH_REMATCH[3]}"
            local command="bash='$0'"
            local params="terminal=false refresh=true font=${ITEM_FONT_NAME:-CourierNew} size=${ITEM_FONT_SIZE:-14}"

            local type_img="$IMG_VPN"
            case "$type" in
                IPSec) type_img="$IMG_CISCO" ;;
                # PPP:L2TP) type_img="$IMG_L2TP"  ;;
                VPN:com.wireguard.macos)  type_img="$IMG_WIREGUARD"  ;;
                VPN:com.cisco.anyconnect) type_img="$IMG_ANYCONNECT" ;;
            esac
            if [[ "$ITEM_DISPLAY_TYPE_ICON" == "1" && -n "${type_img// /}" ]]; then
                params+=" image=${type_img}"
            fi

            case "$status" in
                Disconnected)
                    icon="${ITEM_ICONS[0]}"
                    color="${ITEM_COLORS[0]}"
                    status=""
                    command+=" param1=start param2='${name}'"
                    ;;
                Connected)
                    local data="$(scutil --nc status "$name")"
                    if [[ "$data" =~ 'LastStatusChangeTime : '([0-9:/ ]*) ]]; then
                        status="$( format_connected_time "${BASH_REMATCH[1]}" )"
                    fi
                    icon="${ITEM_ICONS[1]}"
                    color="${ITEM_COLORS[1]}"
                    command+=" param1=stop param2='${name}'"
                    ((count++))
                    ;;
                *)
                    icon="${ITEM_ICONS[2]}"
                    color="${ITEM_COLORS[2]}"
                    command+=" param1=start param2='${name}'"
                    ((progr++))
                    ;;
            esac
            items+=(
                "${name}:  ${icon} ${status} | ${command} color=${color} $params"
            )
        done

        echo -e "$(xbar_icon "$count" "$progr") | color=$(xbar_color "$count" "$progr") font=${MENU_FONT_NAME:-CourierNew} size=${MENU_FONT_SIZE:-14}"
        echo "---"
        for item in "${items[@]}"; do
            echo -e "$item"
        done
    }
}

define_icons() {
    ICON_UNLOCKED='\xf0\x9f\x94\x93' # 🔓
    ICON_LOCKED='\xf0\x9f\x94\x90'   # 🔐
    ICON_KEY='\xf0\x9f\x94\x91'      # 🔑
    ICON_LINK='\xf0\x9f\x94\x97'     # 🔗
    ICON_BOXBOX='\xe2\xa7\x89'       # ⧉
    ICON_ELIPSIS='\xe2\x80\xa6'      # …

    ICON_HOURGLASS_COLOR='\xe2\x8f\xb3' # ⏳
    ICON_HOURGLASS_EMPTY='\xe2\xa7\x96' # ⧖
    ICON_HOURGLASS_FULL='\xe2\xa7\x97'  # ⧗

    ICON_PLUG='\xf0\x9f\x94\x8c'     # 🔌
    ICON_CASTLE='\xe2\x9b\xab'       # ⛫
    ICON_SHIELD_EMPTY='\xe2\x9b\x89' # ⛉
    ICON_SHIELD_CROSS='\xe2\x9b\xa8' # ⛨
    ICON_SHIELD_FULL='\xe2\x9b\x8a'  # ⛊
    ICON_SHIELD_COLOR='\xf0\x9f\x9b\xa1\xef\xb8\x8f' # 🛡️

    ICON_CIRCLED_1='\xe2\x9e\x80'  # ➀
    ICON_CIRCLED_2='\xe2\x9e\x81'  # ➁
    ICON_CIRCLED_3='\xe2\x9e\x82'  # ➂
    ICON_CIRCLED_4='\xe2\x9e\x83'  # ➃
    ICON_CIRCLED_5='\xe2\x9e\x84'  # ➄
    ICON_CIRCLED_6='\xe2\x9e\x85'  # ➅
    ICON_CIRCLED_7='\xe2\x9e\x86'  # ➆
    ICON_CIRCLED_8='\xe2\x9e\x87'  # ➇
    ICON_CIRCLED_9='\xe2\x9e\x88'  # ➈
    ICON_CIRCLED_10='\xe2\x9e\x89' # ➉

    ICON_DICE_1='\xe2\x9a\x80' # ⚀
    ICON_DICE_2='\xe2\x9a\x81' # ⚁
    ICON_DICE_3='\xe2\x9a\x82' # ⚂
    ICON_DICE_4='\xe2\x9a\x83' # ⚃
    ICON_DICE_5='\xe2\x9a\x84' # ⚄
    ICON_DICE_6='\xe2\x9a\x85' # ⚅
}

define_images() {
    IMG_VPN='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'
    IMG_L2TP='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'
    IMG_CISCO='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'
    IMG_WIREGUARD='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'
    IMG_ANYCONNECT='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'
}

main "$@"
