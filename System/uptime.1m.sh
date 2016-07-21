#!/bin/bash

# <bitbar.title>uptime</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>Matteo Ferrando</bitbar.author>
# <bitbar.author.github>chamini2</bitbar.author.github>
# <bitbar.desc>Show uptime command information.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/qaIxpJN.png</bitbar.image>

# the `sed` command removes the occasional leading whitespace
INFO=$(uptime | sed 's/^ *//g')
echo "$INFO" | awk -F'[ ,:\t\n]+' '
    {
        PLURAL = 1
        VERBOSE = 0

        SEP = ", "

        DS = " day"
        HS = " hr"
        MS = " min"
        SS = " sec"

        ################################

        D = H = M = S = 0
        if (substr($5,0,1) == "d") {
        # up for a day or more
            D = $4

            P = $6
            Q = $7
        } else {
            P = $4
            Q = $5
        }

        if (int(Q) == 0) {
        # words evaluate to zero
        # exact times format, like `P = 55`, `Q = secs`
            Q = substr(Q,0,1)

            if (Q == "h") { H = P }
            else if (Q == "m") { M = P }
            else if (Q == "s") { S = P }
        } else {
        # hh:mm format, like `P = 4`, `Q = 20`
            H = P
            M = Q
        }

        MSG = "↑ " include(D, DS, SEP, PLURAL)
        MSG = MSG  include(H, HS, SEP, PLURAL, (D > 0 && VERBOSE))
        MSG = MSG  include(M, MS, SEP, PLURAL, (D > 0 && VERBOSE))
        MSG = MSG  include(S, SS, SEP, PLURAL)

        # remove the remaining SEP
        MSG = substr(MSG, 0, length(MSG) - length(SEP))

        print "[", MSG, "] | size=12"
    }

    function include(VAL, UNIT, SUFFIX, PLURAL, VERBOSE) {
        VAL = int(VAL)

        if (PLURAL && VAL != 1) {
            UNIT = UNIT"s"
        }

        if (VAL > 0 || VERBOSE) {
            return (VAL UNIT SUFFIX)
        } else {
            # return ""
        }
    }'

echo "---"
echo "$INFO" | tr "," "\n" | tail -n 2
