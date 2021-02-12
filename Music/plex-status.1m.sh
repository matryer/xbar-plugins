#!/bin/bash

# <bitbar.title>Plex Status</bitbar.title>
# <bitbar.version>v0.3</bitbar.version>
# <bitbar.author>Chris Bergeron</bitbar.author>
# <bitbar.author.github>chrisbergeron</bitbar.author.github>
# <bitbar.desc>See what's currently playing on your Plex</bitbar.desc>
# <bitbar.image>https://res.cloudinary.com/cyberge/image/upload/v1550627901/icons/plex_878759_eey690.png</bitbar.image>
# <bitbar.dependencies>bash,imagemagick</bitbar.dependencies>

# Put your plex hostname here
plexhost="plex-01"
#curlcmd=$(command -v curl)

# Put host to ping here (useful for vpn connections)
pinghost="plex-01"

# To do: Automate this from a curl response
# The UUID of your Plex Server.  This can be obtained from the browser address bar after selecting an item (it's between /server/ and /detail/)
serverid="YOUR SERVER ID HERE"

# Temp filename
tmpfile="/tmp/image.jpg"
touch $tmpfile
echo > /tmp/output.txt

# To do, retrieve Plex Token automatically, send username/password in Auth Request Header
#plextoken=$(curl -s -X POST \
#  'https://my.plexapp.com/users/sign_in.xml?X-Plex-Client-Identifier=my-app&Content-Length=0' \
#  -H 'cache-control: no-cache')

# Put your Plex API token here
plextoken=YOUR_PLEX_API_TOKEN_HERE

# If host is unreachable, display message and stop
if ! (ping -c1 -W1 $plexhost > /dev/null 2>&1); then
  echo "❌"
  echo "---"
  echo "Can't connect to $pinghost"
  exit
fi

# If we can connect to Plex, let's see what's playing and display it
if ! (curl -s http://$plexhost:32400/identity > /dev/null 2>&1); then
  echo "❌"
  echo "---"
  echo "Plex is unreachable | color=red"
  echo "$plexhost"
fi

# Get our session data from Plex
xmldata=$(curl -s -X GET \
  "http://plex-01:32400/status/sessions?X-Plex-Token=$plextoken" \
  -H 'Content-Length: 0' \
  -H 'X-Plex-Client-Identifier: my-app' \
  -H 'cache-control: no-cache')

echo "XMLDATA is: ($xmldata)" >> /tmp/output.txt

if (echo "$xmldata" | grep "Unauthorized"> /dev/null 2>&1); then
  echo -en "❌\n---\nUnauthorized.\nCheck your API Key."
  exit
fi
  
# Check to see if anything is playing, if not, say so and exit
if ! (echo "$xmldata" | grep "Video" > /dev/null 2>&1); then
  echo "○"
  echo "---"
  echo "Nothing playing"
  exit
fi

# Check to see if a trailer is playing, if so, exit
# To do: Display trailer info
if ! (echo "$xmldata" | grep "subtype=\"trailer\"" > /dev/null 2>&1); then
  echo "D"
  echo "---"
  echo "Preview Trailer Playing"
  exit
fi

# If metadata is a movie, parse it separately
if (echo "$xmldata" | grep "movie" > /dev/null 2>&1); then
  # We're watching a movie, parse as such
  thumblink=$(echo "$xmldata" | sed -e 's/>/\>\n/g' | head -n3 | tail -n1 | cut -f4 -d "\"" | sed -e 's/art/thumb/g')
  keydata="%2Flibrary%2Fmetadata%2F"$(echo "$thumblink" | cut -f4 -d "/")
else
  # We're watching TV, parse as such
  thumblink=$(echo "$xmldata" | sed -e 's/" /\n/g' | grep grandparentThumb | cut -f2 -d "\""| head -n1)
  keydata=$(echo "$xmldata" | sed -e 's/" /\n/g' | grep "key" | head -n1 | cut -f2 -d "\"" | sed -e 's/\//%2F/g' | head -n1)
fi

# Debug info
#echo "thumbnail link is: ($thumblink)" >> /tmp/output.txt
#echo "keydata for url is: ($keydata)" >> /tmp/output.txt

curl -s -o /tmp/image.jpg -X GET \
  "http://plex-01:32400$thumblink?X-Plex-Token=$plextoken" \
  -H 'Content-Length: 0' \
  -H 'X-Plex-Client-Identifier: my-app' \
  -H 'cache-control: no-cache'

# Resize our image to a small thumbnail
#mogrify -scale 75x100 $tmpfile
if ! (mogrify -scale 75x100 /tmp/image.jpg >> /tmp/output.txt 2>&1); then
  echo "---"
  echo "Couldn't render thumbnail image"
  exit
fi

# Encode our image into base64 text for BitBar
imgstr=$(base64 /tmp/image.jpg)

# Display our menu output
echo "| templateImage=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAAC4kAAAuJATfJy60AAAAHdElNRQfjAhMPKzgxRmpXAAAAuUlEQVQoz5XRMU4CYRQE4O93LbCFtZDQ2dGItS2n4AKchFNwAbnIVhYmW1GTEBJDgiGRbh+FGyPwR+I0r5iZvHlvgGSmEcLeUBalWghhofubKNr55dNYBw823sW5gLW+Z0lH6c3mUnCw8mQgGbhVOZwL+FB4cSd5tFTnonYt2qi1Mn/N0F4IjZkEN/6FKysKU1sh7Exy/pFKIzTmepd0z7ylK6Ocf2InhK3pyXda3P+U9Xpa1jf+qPsIyJ5B7rJwvZwAAAAldEVYdGRhdGU6Y3JlYXRlADIwMTktMDItMTlUMjA6NDQ6MDAtMDU6MDBDoNoZAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDE5LTAyLTE5VDIwOjQzOjU2LTA1OjAw+xFCggAAAABJRU5ErkJggg=="
echo "---"
#  echo "Now Playing | color=green"
echo "Now Playing | color=orange"
echo "| image=$imgstr href=http://plex-01:32400/web/index.html#!/server/$serverid/details?key=$keydata"
