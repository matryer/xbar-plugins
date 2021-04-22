#!/bin/bash

#  <xbar.title>Password Store</xbar.title>
#  <xbar.version>v1.0.1</xbar.version>
#  <xbar.author>ri7nz</xbar.author>
#  <xbar.author.github>ri7nz</xbar.author.github>
#  <xbar.desc>password store with xbar</xbar.desc>
#  <xbar.image>https://github.com/ri7nz/x-pass-plugin/blob/master/xbar.image.png?raw=true</xbar.image>
#  <xbar.dependencies>pass,gpg,pinentry-mac</xbar.dependencies>
#  <xbar.abouturl>https://github.com/ri7nz/x-pass-plugin</xbar.abouturl>

# maybe you need change the path of pass or cut bin
# use where if you didn't know where pass or cut path
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:$PATH"

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
  while IFS= read -r -d '' singelfile
  do
    singelfile="${singelfile%.*}"
    echo "$(basename "$singelfile") | bash='$0' param1='${singelfile}' terminal=false"
  done < <(find "." -maxdepth 1 -type f -name '*.gpg' -print0)
  
  while IFS= read -r -d '' dirname
  do
    dirname=${dirname:2}
    echo "${dirname}"
      while IFS= read -r -d '' filename
      do
        filename="${filename%.*}"
        echo "-- $(basename "$filename") | bash='$0' param1='${filename}' terminal=false"
      done < <(find "$dirname" -type f -name '*.gpg' -print0)
  done < <(find ./* -maxdepth 1 -type d -print0)
else
  echo "pass or cut insn't installed, please install"
fi
