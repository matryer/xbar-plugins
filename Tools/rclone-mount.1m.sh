#!/bin/bash

# <xbar.title>Rclone Mount</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ryan Chiechi</xbar.author>
# <xbar.author.github>rchiechi</xbar.author.github>
# <xbar.desc>Quickly mount endpoints in rclone.</xbar.desc>
# <xbar.image>https://i.imgur.com/woYDSdX.png</xbar.image>
# <xbar.dependencies>bash, rclone</xbar.dependencies>


#TARGET is the local mountpoint for the remote
TARGET=${HOME}/encrypted
#Homebrew installs rclone /usr/local/bin
RCLONE=/usr/local/bin/rclone
# If your rclone config is not encrypted, comment out the next two lines
RCLONEPASS=${HOME}/.password-store/Macbert/rclone.gpg
export RCLONE_CONFIG_PASS=$(/usr/local/bin/gpg -o - -d "${RCLONEPASS}" 2>/dev/null)

# You should not need to edit anythinng below here. 
function rcloneimage() {
  icon="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"
}

function mountpoint() {
  mount | grep -q "$1"
  return $?
}

function remotemounted() {
  while IFS= read -r remote; do
    mountpoint "${remote}" && return 0
  done < <(${RCLONE} --ask-password=false listremotes)
  return 1
}

function isremote() {
  while IFS= read -r remote; do
    [[ "$1" == "${remote}" ]] && return 0
  done < <(${RCLONE} --ask-password=false listremotes)
  return 1
}

function domount() {
  mountpoint "${TARGET}" && return 1
  ${RCLONE} --ask-password=false mount --daemon "${1}" "${TARGET}" >/dev/null 2>&1
  RETURN=$?
  sleep 5
  return
}

function dounmount() {
  diskutil unmount ${TARGET} >/dev/null 2>&1
  RETURN=$?
  sleep 5
  return
}

function listremotes() {
  echo "---"
  ${RCLONE} --ask-password=false listremotes | while read remote; do
    _item=$(printf "${remote}" | tr -d ':')
    if mountpoint ${remote} ; then
      color="color=green"
      emoji=":floppy_disk:"
    else
      color=""
      emoji=""
    fi
    echo "${emoji}  ${_item}| bash='$0' param1=${remote} refresh=true terminal=false ${color}"
  done
}

if [[ -n "$1" ]] && ! isremote "$1"; then
  exit 1
elif [[ -n "$1" ]] && mountpoint "$1" ; then
  dounmount
  exit $?
elif [[ -n "$1" ]] && remotemounted "$1" ; then
  dounmount && domount "$1"
  exit $?
elif [[ -n "$1" ]]; then
  domount "$1"
  exit $?
fi

# Output menu items
rcloneimage
echo " | image=${icon}"
listremotes
