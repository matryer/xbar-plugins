#!/bin/bash
# <xbar.title>Delete VSCode Temp Files</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author.github>AlexPoulsen</xbar.author.github>
# <xbar.author>AlexPoulsen</xbar.author>
# <xbar.desc>Deletes Visual Studio Code Temporary Files</xbar.desc>

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