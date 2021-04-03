#!/bin/bash
# <xbar.title>website-status</xbar.title>
# <xbar.author>Marc Oehler</xbar.author>
# <xbar.author.github>kemar220</xbar.author.github>
# <xbar.desc>Gets the status of your website</xbar.desc>
# <xbar.version>1.0</xbar.version>

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
