#!/bin/bash
# <bitbar.title>website-status</bitbar.title>
# <bitbar.author>Marc Oehler</bitbar.author>
# <bitbar.author.github>kemar220</bitbar.author.github>
# <bitbar.desc>Gets the status of your website</bitbar.desc>
# <bitbar.version>1.0</bitbar.version>

url='http://www.kemar.ch' # replace with your url

code=$(curl -o /dev/null --silent --head --write-out '%{http_code}\n' $url)

case "$code" in
"200")
    echo "ok"
    ;;
"301" | "302")
    echo "redirected"
    ;;
*)
    echo "error"
    ;;
esac

echo "---"
echo $url
echo "http code $code"
