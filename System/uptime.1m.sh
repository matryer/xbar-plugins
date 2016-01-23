#!/bin/bash

# <bitbar.title>uptime</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Matteo Ferrando</bitbar.author>
# <bitbar.author.github>chamini2</bitbar.author.github>
# <bitbar.desc>Show uptime command information.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/qaIxpJN.png</bitbar.image>

INFO=`uptime`
echo $INFO | awk -F'[ ,:\t\n]+' '{
        msg = "↑ "
        if ($5 == "day" || $5 == "days") {      # up for a day or more
            msg = msg $4 $5 ", "

            n = $6
            o = $7
        } else {
            n = $4
            o = $5
        }

        if (int(o) == 0) {                      # words evaluate to zero
            msg = msg int(n)" "o
        } else {                                # hh:mm format
            msg = msg int(n)" hr"
            if (n > 1) { msg = msg "s" }

            msg = msg ", " int(o) " min"
            if (o > 1) { msg = msg "s" }
        }

        print "[", msg, "]"
    }'
echo "---"
echo $INFO | tr "," "\n" | tail -n 2
