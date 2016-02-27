#!/bin/bash

# <bitbar.title>Homebrew Cask Updater</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Srdgh</bitbar.author>
# <bitbar.author.github>srdgh</bitbar.author.github>
# <bitbar.desc>List out-of-date apps i.e. apps for whch a newer Homebrew cask exists. Click menu item to install cask.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/kBf90TB.png?1</bitbar.image>

brewupdate=$(/usr/local/bin/brew update) # performs 'brew update' and captures output. Currently does nothing with output. Use this variable to add further information to plugin (e.g. new non-cask formulae which have been updated)

brewcasklist=$(/usr/local/bin/brew cask ls -1 | sed 's_(!)__g' | xargs /usr/local/bin/brew cask info | grep -A 1 'Not installed' | sed -e 's_Not installed__g' -e 's_https://github\.com/caskroom/homebrew-cask/blob/master/Casks/__g' -e 's_\.rb__g')

brewcasknum=$(for line in $brewcasklist; do echo $line | grep "[a-z]" ; done | wc -w | xargs)

if [[ "${brewcasknum}" != "0" ]]; then
if [[ "${brewcasknum}" == "1" ]]; then
echo "🍺"
else
echo "🍻"
fi
echo "---"
echo $brewcasknum "casks to update"
for line in $brewcasklist; do echo $line | grep "[a-z]" | sed 's_\(.*\)_Update & | bash=\"brew cask install &\" terminal=true refresh=_g' ; done
echo "---"
# Uncomment following lines to add the commands to the drop-down menu
# echo "Brew Update | bash='brew update' terminal=true refresh="
echo "Brew Upgrade | bash='brew upgrade' terminal=true refresh="
# echo "Brew Cleanup | bash='brew cleanup' terminal=true refresh="
# echo "Brew Cask Cleanup | bash='brew cask cleanup' terminal=true refresh="
echo "Refresh | refresh="
fi
