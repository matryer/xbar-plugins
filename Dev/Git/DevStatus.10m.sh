#!/bin/bash

# <xbar.title>Dev Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Abbey Hawk Sparrow</xbar.author>
# <xbar.author.github>khrome</xbar.author.github>
# <xbar.desc>Recursive status of any discovered .git repos in the provided directory. </xbar.desc>
# <xbar.var>string(DIRECTORY="/Users/khrome/Development"): Directory to use.</xbar.var>
# <xbar.image></xbar.image>
# <xbar.dependencies></xbar.dependencies>

echo "🚧"
echo "---"
GITDIRTY_ISSUE=""
GITDIRTY_HIGHLIGHT=""
GITDIRTY="❌"
GITCLEAN="✅"
DIRTYPOST=" | color=red"
CLEANPOST=" | color=white"
GITDIRTY_NORMAL=''
GITCLEAN_NORMAL=''

#DIRECTORY='/Users/khrome/Development'

catch() {
  echo "Error $1 occurred on $2"
}

gitdirty(){
  local d="$1"
  trap 'catch $? $LINENO' ERR
  if [ -d "$d" ]; then
    if [ -e "$d/.ignore" ]; then
      echo -e "";
    else
      if [[ "${d:0:1}" == "-" ]]; then
          echo ""
      else
          cd $d > /dev/null
          if [ -d ".git" ]; then
            DIRTYDIR="${d}"
            ISDIRTY=$(git diff --shortstat 2> /dev/null | tail -n1)
            OPENOPTS="| shell=open | param1=\"$DIRECTORY/$d\""
            [[ $ISDIRTY != "" ]] && printf " ${GITDIRTY_ISSUE}${GITDIRTY}${GITDIRTY_NORMAL}%-26s $DIRTYPOST $OPENOPTS \n" "$DIRTYDIR"
            [[ $ISDIRTY == "" ]] && printf " ${GITDIRTY_HIGHLIGHT}${GITCLEAN}${GITCLEAN_NORMAL}%-26s $CLEANPOST $OPENOPTS \n" "$DIRTYDIR"
          else
            gitdirtyrepos *
          fi
      fi
      cd .. > /dev/null
    fi
  fi
  #echo "Exiting update: pwd=`pwd`"
}

gitdirtyrepos(){
  #echo "`pwd`"
  for x in $*; do
    gitdirty "$x"
  done
}

set -e
trap 'case $? in
        139) echo "segfault occurred";;
        11) echo "ssegfault occurred";;
      esac' EXIT
gitdirtyrepos "$DIRECTORY"
