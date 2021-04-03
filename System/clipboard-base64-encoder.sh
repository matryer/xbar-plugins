#!/bin/bash

# <xbar.title>Clipboard BASE64-Encoder</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Steffen Froehlich</xbar.author>
# <xbar.author.github>citoki</xbar.author.github>
# <xbar.desc>This plugin will encode acutal clipboard item with BASE64.
# The result will be available in clipboard again. Do not use the result
# for cryptographic purposes!!
# It's inspired by the original clipboard-history plugin</xbar.desc>
# <xbar.image>https://imgur.com/0Ym6xNC</birtbar.image>
# <xbar.dependencies></xbar.dependencies>

# Hack for language not being set properly and unicode support
export LANG="${LANG:-en_US.UTF-8}"

PREFIX=""
APPENDIX=""

# user clicks on 'encoding'
if [[ "$1" = "encode" ]]; then
  echo -n "$(echo -n "${PREFIX}$(pbpaste)${APPENDIX}" | base64)" | pbcopy
  osascript -e "display notification \"Clipboard entry encoded with BASE64\" with title \"BitBar Clipboard BASE64-Encoder\"" &> /dev/null
  exit
fi

if [[ "$1" = "decode" ]]; then
  echo -n "$(echo -n "${PREFIX}$(pbpaste)${APPENDIX}" | base64 -D)" | pbcopy
  osascript -e "display notification \"Clipboard entry decoded with BASE64\" with title \"BitBar Clipboard BASE64-Encoder\"" &> /dev/null
  exit
fi


# Print icon
echo "🔏"
echo "---"
echo "Encode clipboard item with BASE64 | bash='$0' param1=encode terminal=false"
echo "Decode clipboard item with BASE64 | bash='$0' param1=decode terminal=false"

