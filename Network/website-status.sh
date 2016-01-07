# website-status
# BitBar plugin
#
# Author Marc Oehler
#
# Gets the status of your website


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
