#!/usr/local/bin/bash
# shellcheck disable=SC2154

# <bitbar.title>Home Config</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kodie Grantham</bitbar.author>
# <bitbar.author.github>kodie</bitbar.author.github>
# <bitbar.desc>Example of how to mimic the functionality of the home-config npm module in a bash BitBar plugin</bitbar.desc>
# <bitbar.dependencies>bash4</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/kodie/bitbar-home-config</bitbar.abouturl>

typeset -A cfg
cfg[home_config,color]="red"
cfg[home_config,text]="Try editing $HOME/.bitbarrc"

cfgFile="$HOME/.bitbarrc"
if [ ! -e "$cfgFile" ]; then touch "$cfgFile"; fi
while read -r cfgLine; do
  if [[ -z $cfgLine ]]; then continue; fi
  if [[ ${cfgLine:0:1} == '[' ]]; then cfgKey=${cfgLine:1:-1};
  else IFS='='; cfgVar=($cfgLine); unset IFS; cfg[$cfgKey,${cfgVar[0]}]=${cfgVar[1]}; fi
done < "$cfgFile"

echo "Config Example | dropdown=false color=${cfg[home_config,color]}"
echo "---"
echo "${cfg[home_config,text]}"
