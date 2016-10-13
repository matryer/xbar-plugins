#!/bin/bash

# <bitbar.title>Homebrew Cask Updater</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Srdgh</bitbar.author>
# <bitbar.author.github>srdgh</bitbar.author.github>
# <bitbar.desc>List out-of-date apps i.e. apps for whch a newer Homebrew cask exists. Click menu item to install cask.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/kBf90TB.png?1</bitbar.image>

#I've taken the next six lines from the brew update plugin.
exit_with_error() {
  echo "err | color=red";
  exit 1;
}

/usr/local/bin/brew update &> /dev/null || exit_with_error; 

brewcasklist=$(/usr/local/bin/brew cask ls -1 | sed 's_(!)__g' | xargs /usr/local/bin/brew cask info | grep -A 1 'Not installed' | sed -e 's_Not installed__g' -e 's_From: https://github\.com/caskroom/homebrew-cask/blob/master/Casks/__g' -e 's_\.rb__g');

brewcasknum=$(for line in $brewcasklist; do echo "$line" | grep "[a-z]" ; done | wc -w | xargs);

if [[ "${brewcasknum}" != "0" ]]; then
if [[ "${brewcasknum}" == "1" ]]; then
echo "🍺"
echo "---"
echo "$brewcasknum cask to update"
else
echo "🍻"
echo "---"
echo "$brewcasknum casks to update"
fi
for line in $brewcasklist; do echo "$line" | grep "[a-z]" | sed 's_\(.*\)_& | bash=brew param1=cask param2=install param3=& terminal=true refresh=_g' ; done
fi
if [[ "${brewcasknum}" == "0" ]]; then
echo "🍹"
echo "---"
echo "No casks to update"
fi
echo "---"
# Uncomment following lines to add the commands to the drop-down menu
# echo "Brew Update | bash=brew param1=update terminal=true refresh="
echo "Brew Upgrade | bash=brew param1=upgrade terminal=true refresh="
# echo "Brew Cleanup | bash=brew param1=cleanup terminal=true refresh="
# echo "Brew Cask Cleanup | bash=brew param1=cask param2=cleanup terminal=true refresh="
echo "Refresh | refresh="
