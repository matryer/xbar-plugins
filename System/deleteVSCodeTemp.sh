#!/bin/bash
# <bitbar.title>Delete VSCode Temp Files</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author.github>AlexPoulsen</bitbar.author.github>
# <bitbar.author>AlexPoulsen</bitbar.author>
# <bitbar.desc>Deletes Visual Studio Code Temporary Files</bitbar.desc>

if [ "$1" = 'clean' ]; then
  sudo find /Users/macbookpro/Documents/VSCode\ Projects/ -name "temp*" -d -ok rm {} \;
  # shellcheck disable=SC2162
  read -p "Files are about to be deleted, continue? [Y/n] " check
  if [[ ( $check == "Y" ) || ( $check == "y" ) ]]; then
    sudo find /Users/macbookpro/Documents/VSCode\ Projects/ -name "temp*" -d -exec rm {} \;
    echo "Deleted from default directory"
  else
    echo "Files not deleted"
  fi
fi

echo "🗑"
echo '---'
echo "Clean Temp Files | bash='$0' param1=clean terminal=true"