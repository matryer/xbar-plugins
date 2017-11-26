#!/usr/bin/env bash

# JSON Utils: format or compact JSON string from clipboard and then write to clipboard
#
# by Cnfn (http://github.com/cnfn)
#
# <bitbar.title>JSON Utils</bitbar.title>
# <bitbar.version>v1.3</bitbar.version>
# <bitbar.author>Cnfn</bitbar.author>
# <bitbar.author.github>cnfn</bitbar.author.github>
# <bitbar.desc>format or compact JSON string from clipboard and then write to clipboard</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/cnfn/grocery/master/images/blog/bitbar_plugin_json_utils_main.png</bitbar.image>
# <bitbar.dependencies>bash,jq</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/cnfn/bitbar-plugins/tree/master/Tools/JsonUtils</bitbar.abouturl>
#
# Dependencies:
#   jq (https://stedolan.github.io/jq/)

export PATH=$PATH:/usr/local/bin

# default env is UTF-8, avoid garble
export LANG=en_US.UTF-8

notifyTitle="JsonUtils"
notifyValidJson="Valid JSON, type:"
notifyInvalidJson="Invalid JSON !!!!!!!!!!"

displayNotification() {
	title=$1
	content=$2
	osascript -e "display notification \"$content\" with title \"$title\""
}

doValidate() {
	typeName=$(pbpaste | jq type 2> /dev/null | xargs echo -n 2> /dev/null)
	([ "object" == "$typeName" ] || [ "array" == "$typeName" ]) && echo $typeName || echo ""
}

notifyAndExitWhenInvalidJson() {
	typeName=$(doValidate)
	[ -n "$typeName" ] || { osascript -e "beep"; \
		displayNotification $notifyTitle "$notifyInvalidJson"; exit 1; }
}

validate() {
	typeName=$(doValidate)
	[ -n "$typeName" ] && displayNotification $notifyTitle "$notifyValidJson $typeName" || \
		{ osascript -e "beep"; displayNotification $notifyTitle "$notifyInvalidJson"; }
}

format() {
	notifyAndExitWhenInvalidJson

	pbpaste | jq . --indent 4 | pbcopy
	displayNotification $notifyTitle "Formatted"
}

compact() {
	notifyAndExitWhenInvalidJson

	pbpaste | jq . --compact-output | pbcopy
	displayNotification $notifyTitle "Compacted"
}

# call function: validate, format, compact
[ $# == 1 ] && { $1; exit 0; }

echo "JSON"
echo "---"
echo "Validate | bash='$0' param1=validate terminal=false"
echo "---"
echo "Format | bash='$0' param1=format terminal=false"
echo "Compact | bash='$0' param1=compact terminal=false"
