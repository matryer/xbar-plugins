#!/bin/bash

# <bitbar.title>Homebrew Cask Updater</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Srdgh</bitbar.author>
# <bitbar.author.github>srdgh</bitbar.author.github>
# <bitbar.desc>List out-of-date apps i.e. apps for whch a newer Homebrew cask exists. Click menu item to install cask.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/a0OE9Mn.png?1</bitbar.image>

brewcall=$(/usr/local/bin/brew cask ls -1 | sed 's/(!)//g' | xargs /usr/local/bin/brew cask info | grep -A 1 'Not installed' | sed -e 's_Not installed__g' -e 's_https://github\.com/caskroom/homebrew-cask/blob/master/Casks/__g' -e 's_\.rb__g')

brewnum=$(for line in $brewcall; do echo $line | grep "[a-z]" ; done | wc -w)

echo "${brewnum}"
echo "---"
echo $brewnum "casks to update"
if [[ "${brewnum}" -ne "0" ]]
then 
echo "Click item to install new version:"
fi
for line in $brewcall; do echo $line | grep "[a-z]" | sed 's/\(.*\)/& | bash=\"brew cask install &\" terminal=true refresh=/g' ; done
echo "---"
echo "Brew Update | bash='brew update' terminal=true refresh="
echo "Brew Upgrade | bash='brew upgrade' terminal=true refresh="
echo "Refresh | refresh="
