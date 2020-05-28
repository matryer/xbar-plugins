#!/bin/bash

# <bitbar.title>Copy SSH keys</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Bastian Kersting</bitbar.author>
# <bitbar.author.github>1c3t3a</bitbar.author.github>
# <bitbar.desc>Provides a quick way to copy your public keys to the clipboard</bitbar.desc>
# <bitbar.image>https://imgur.com/GYOuLJ1</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

if [[ $1 ]]; then
    cd ~/.ssh && pbcopy < "$1"
fi

# display icon
echo "🧬"
echo "---"

# navigate to key directory (provide the location if the plugin doesn't work)
cd ~/.ssh || echo "Please configure ssh key location"
for filename in *.pub;do
    echo "${filename} | bash='$0' param1='${filename}' terminal=false"
done
