#!/usr/bin/env bash
# <xbar.title>Dev Toolbox</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ankit Gaurav</xbar.author>
# <xbar.author.github>theankitgaurav</xbar.author.github>
# <xbar.desc>Handy menu bar app to serve as swiss-army knife for devs.</xbar.desc>
#  <xbar.image>https://user-images.githubusercontent.com/9376908/185213187-c6ad3f67-217c-47a4-9283-10d92b1f2aa9.png</xbar.image>
# <xbar.dependencies>shell,jq,sed</xbar.dependencies>
# <xbar.abouturl>https://github.com/theankitgaurav/dev-toolbox</xbar.abouturl>

# Variables as preferences of the app:
#  <xbar.var>select(APP_ICON="⚙️"): App icon to be shown in the menu app. [⚙️,🛠,DevToolBox]</xbar.var>

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:$PATH"
SCRIPT_PATH="$(realpath -s $0)"

function createHeader() {
  echo "$1"
  echo "---"
  echo "$2"
}
function createMenu() {
  echo "$1 | refresh=true "
}
function createAction() {
  echo "-- $1 | refresh=true terminal=false bash='$SCRIPT_PATH' param1=$2 param2='|' param3='pbcopy'"
  echo "-- $1 | refresh=true terminal=true alternate=true bash='$SCRIPT_PATH' param1=$2"
}

function prettifyJson() {
  pbpaste | jq .
}
function minifyJson() {
  pbpaste | jq -c .
}
function escapeJson() {
  pbpaste | jq @json
}
function unEscapeJson() {
  pbpaste | jq -r
}
function encodeBase64() {
  pbpaste | base64
}
function decodeBase64() {
  pbpaste | base64 -d
}
function unixToLocalTimestamp() {
  date -r "$(pbpaste)" +%Y-%m-%dT%H:%M:%SZ
}
function unixToUtcTimestamp() {
  date -r "$(pbpaste)" -u +%Y-%m-%dT%H:%M:%SZ
}
function currentUnixTimestamp() {
  date +%s
}
function encodeUrl() {
  pbpaste | jq -sRr @uri
}
function decodeUrl() {
  pbpaste | printf "%b\n" "$(sed 's/+/ /g; s/%\([0-9a-f][0-9a-f]\)/\\x\1/g;')";
}
function generateUuid() {
  uuidgen
}

[ $# -ge 1 ] && { $1 && exit $?; }

createHeader "$APP_ICON" 'Dev Toolbox'

createMenu "JSON"
createAction "Format" prettifyJson
createAction "Minify" minifyJson
createAction "Escape" escapeJson
createAction "Unescape" unEscapeJson

createMenu "Base64"
createAction "Encode" encodeBase64
createAction "Decode" decodeBase64

createMenu "Date"
createAction "Unix timestamp -> Local timestamp" unixToLocalTimestamp
createAction "Unix timestamp -> UTC timestamp" unixToUtcTimestamp
createAction "Current Unix timestamp" currentUnixTimestamp

createMenu "URL"
createAction "Encode" encodeUrl
createAction "Decode" decodeUrl

createMenu "UUID"
createAction "Generate" generateUuid
