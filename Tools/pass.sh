#!/bin/bash

#  <xbar.title>Password Store</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>ri7nz</xbar.author>
#  <xbar.author.github>ri7nz</xbar.author.github>
#  <xbar.desc>password store with xbar</xbar.desc>
#  <xbar.image>https://github.com/ri7nz/x-pass-plugin/blob/master/xbar.image.png?raw=true</xbar.image>
#  <xbar.dependencies>pass,cut,gpg</xbar.dependencies>
#  <xbar.abouturl>https://github.com/ri7nz/x-pass-plugin</xbar.abouturl>

# maybe you need change the path of pass or cut bin
# use where if you didn't know where pass or cut path
export PATH="/opt/homebrew/bin:/usr/bin:$PATH"

echo "Pass"
# first check dependencies is exist 
# when exist print all password-store filename without extension
# when user click some from list set variable param1 
# when param1 have a value run command pass -c (copy to clipboard)
# done
if (command -v cut && command -v pass)&>/dev/null;then
  # get user input (when user click some in list)
  [[ $1 ]] && pass -c "$1"
  echo "---"
 
  # location of password-store file
  cd ~/.password-store || exit
  for filename in **/*.gpg; do
    # remove file extension
    name=$(echo "$filename" | cut -f 1 -d '.')
    echo "${name} | bash='$0' param1='${name}' terminal=false"
  done
else
  echo "pass or cut insn't installed, please install"
fi
