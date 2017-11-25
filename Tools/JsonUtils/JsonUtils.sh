#!/usr/bin/env bash

# JSON Utils: format or compact JSON string from clipboard and then write to clipboard
#
# by Cnfn (http://github.com/cnfn)
#
# <bitbar.title>JSON Utils</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Cnfn</bitbar.author>
# <bitbar.author.github>cnfn</bitbar.author.github>
# <bitbar.desc>format or compact JSON string from clipboard and then write to clipboard</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/cnfn/grocery/master/images/blog/bitbar_plugin_json_utils.png</bitbar.image>
# <bitbar.dependencies>bash,jq</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/cnfn/bitbar-plugins/tree/master/Tools/JsonUtils</bitbar.abouturl>
#
# Dependencies:
#   jq (https://stedolan.github.io/jq/)

export PATH=$PATH:/usr/local/bin

format() {
	pbpaste | jq "." --indent 4 | pbcopy
}

compact() {
	pbpaste | jq "." --compact-output | pbcopy
}

[[ $1 == "format" ]] && { format; exit 0; }
[[ $1 == "compact" ]] && { compact; exit 0; }

echo "JSON"
echo "---"
echo "Format | bash='$0' param1=format terminal=false"
echo "Compact | bash='$0' param1=compact terminal=false"
