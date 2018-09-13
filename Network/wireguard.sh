#!/bin/bash
# <bitbar.title>Wireguard</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Daniel Barber</bitbar.author>
# <bitbar.author.github>danbee</bitbar.author.github>
# <bitbar.desc>Manages a Wireguard VPN connection</bitbar.desc>
# <bitbar.image>https://f001.backblazeb2.com/file/bitbar/wireguard.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

# You will need to add the following line to your sudoers file. Remember to edit
# sudoers with `sudo visudo`.
#
# %admin  ALL=(ALL) NOPASSWD: /usr/local/bin/wg-quick
#
# Rename WG_CONFIG if your Wireguard config is not called `wg0-client`

PATH="/usr/local/bin:$PATH"

WG_CONNECTED="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAY1BMVEVHcEwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD////Iv5pwAAAAH3RSTlMAA0yMvd6sez0V7/qzbglj5qR0Ncj1KA7VLlQflkGEXYPQGAAAAAFiS0dEILNrPYAAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAAHdElNRQfiCB0GISSxSlkqAAABpklEQVQ4y2VT7YKEIAik1GyzTNus1nbz/d/y8AO3u+OXIMLMgABkTcu4EJ1kvJN9A3/twYYQghqnHvQ8BMMev+/neB2sZEt8++SrcvJWpbEhmdiy3wL0IfBapOlCMZkDezsL9A7KWMq1aVPVThhzpADPXTZVEpzgL/Q9+VRxrX44WB8xCvJdbNKHu6klYhjvJSrCsE7PrbV2R9iUITCbOqo5Qz7fmPEYSrQHT+99FfV9qyvhXU42jcPja/hsX+pj5fDEp8h+QKIPLCYrCFMQ4EuWxMkVqLADV1TEhLEcIjeSQgEdUNUrHrpmRnU+lTsQyw+ARuhumTWireq6WmuMDL31GghNAcnpiDpNrySEnkxNGCvcEK4p7Yv2vtlIyMCqkmaScZInW8/YK1RoZRajhileIOsB8ZI8pinsD8SGIHZH0dI5jiVRZpIPKHCbwy+ahUqoaPa+Ls9Js2B5AQYCDM1RxgGWGkfr8zwEtj5Rlg5HrmPInLQifcY8pT9RpTy279c6Ew7V1p+ITty8m3061ENde9654Ozr3//ePeOHsNfCxsvrGv4BOvFCaHw59FkAAAAldEVYdGRhdGU6Y3JlYXRlADIwMTgtMDgtMjlUMTA6MzM6MzYtMDQ6MDCYYoC0AAAAJXRFWHRkYXRlOm1vZGlmeQAyMDE4LTA4LTI5VDEwOjMzOjM2LTA0OjAw6T84CAAAAABJRU5ErkJggg=="
WG_DISCONNECTED="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAY1BMVEVHcEwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD////Iv5pwAAAAIHRSTlMAAhsxQk09K0Q6FgEHVFk/JVEoEklWDyAKBBkNHTUfLpaxL30AAAABYktHRCCzaz2AAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4ggdBiEgtiedMwAAAaFJREFUOMtdU1GigyAMKyIiKihOVDY373/LVypl7vVHSjFNEwDgEJWslWq0bI3uevgfg7QOY5w6EJN1Xg6/9YnKRstZYPaolxD0DUUYR6HWK68AOufaAtI3LofO/apNYbbwiTmXfUWojfJe0UYtqL6GfCCo+oH5xjkjLiV3i+wSR8V52IHo3GPGrTjeIZrv/9NjfRoTcSw+oXoYSn26KL8knhhs3uzg4PrBU+/vG66Gd14ZsuOTRn+u39FHaPMKBxxwHpu+CKaZBOSRAmokSZwLgYEtZFU89/XQp9mKFMALVPVM30ZM+10cRsB5ICJEmDc8KpiZCwWrSRN+zEH+yAKgyhRJp+m6D2L25cBY6Dp3zq9UjtshVhbSyaKkn3VycpdtcrByhVrMFCIkHw/kbPFgn5t4lIeMW5DbhhrSTF4UoZItT2qla3vkNcmevSBW7P1W9HmxF/Ky1zJhEEu2A+ghqHhdgO5SU2HrvUa+aHlMP/kXX5HOl+sIMbKU/IyoCxkZnpxvaYD48zi7JvU5aXN4O2tW+B/DJz1/c85yPI/v339ndyeoPSR2pwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxOC0wOC0yOVQxMDozMzozMy0wNDowMMparxMAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTgtMDgtMjlUMTA6MzM6MzItMDQ6MDAdcBwbAAAAAElFTkSuQmCC"

WG_EXECUTABLE=/usr/local/bin/wg-quick

WG_CONFIG="wg0-client"

WG_PIDFILE="/var/run/wireguard/$WG_CONFIG.name"

case "$1" in
    connect)
        sudo $WG_EXECUTABLE up $WG_CONFIG

        # Wait for connection so menu item refreshes instantly
        until [ -f $WG_PIDFILE ]; do sleep 1; done
        ;;
    disconnect)
        sudo $WG_EXECUTABLE down $WG_CONFIG

        # Wait for disconnection so menu item refreshes instantly
        until [ ! -f $WG_PIDFILE ]; do sleep 1; done
        ;;
esac

if [ -f $WG_PIDFILE ]; then
    echo "| templateImage=$WG_CONNECTED"
    echo '---'
    echo "Disconnect Wireguard | bash='$0' param1=disconnect terminal=false refresh=true"
    exit
else
    echo "| templateImage=$WG_DISCONNECTED"
    echo '---'
    echo "Connect Wireguard | bash='$0' param1=connect terminal=false refresh=true"
    exit
fi
