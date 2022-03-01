#!/usr/bin/env bash

# <xbar.title>NTFS Mounter</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>xfangfang</xbar.author>
# <xbar.author.github>xfangfang</xbar.author.github>
# <xbar.desc>Mount NTFS Drivers</xbar.desc>
# <xbar.image>https://xfangfang.github.io/assets/img/ntfs_mounter.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://xfangfang.github.io/025</xbar.abouturl>
# <xbar.var>string(VAR_PATH="$HOME/ntfs-volume"): mount path(root path is $HOME).</xbar.var>
# <xbar.var>string(VAR_PASSWORD=""): system password(optional).</xbar.var>

# This plugin is only tested with MacOS 12.0.1
# usage:
# bash ntfs.1d.sh
# bash ntfs.1d.sh mount disk2s1 Kingston
# bash ntfs.1d.sh mount disk2s1 Kingston system_password
# bash ntfs.1d.sh umount disk2s1
# bash ntfs.1d.sh open # open volume folder

if [ ! $VAR_PATH ]; then
  VAR_PATH=$HOME/ntfs-volume
fi

if [ ! $1 ]; then
  # gui
  echo NTFS
  echo ---
  echo Refresh \| refresh=true
  echo Open Folder \| shell=\"`pwd`/$0\" param1=open
  if [ ! $VAR_PASSWORD ]; then
    echo You need to enter your password manually
  else
    echo Password Loaded
  fi
  SYSTEM_PROFILER=$(system_profiler SPStorageDataType 2>/dev/null)
  Volume_Num=$(grep "File System: NTFS"<<<"${SYSTEM_PROFILER}" | wc -l)
  if [ $Volume_Num -gt 0 ]; then
    INFO=$(grep -b3 "File System: NTFS"<<<"${SYSTEM_PROFILER}")
    BSD_Name=$(awk '/BSD Name/{printf "%s\\", $4}'<<<"${INFO}")
    Free=$(awk '/Free/{printf "%s\\", $3$4}'<<<"${INFO}")
    Capacity=$(awk '/Capacity/{printf "%s\\", $3$4}'<<<"${INFO}")
    Volume_Name=$(awk -F ': ' '/Mount Point/{print $2}'<<<"${INFO}" | awk -F '/' '{printf "%s\\", $NF}')
    Writable=$(awk '/Writable/{printf "%s\\", $3}'<<<"${INFO}")
    for I in `seq 1 ${Volume_Num}`; do
        Volume_Name_tmp=$(awk -F '\\' '{print $'$I'}' <<< "${Volume_Name}")
        BSD_Name_tmp=$(awk -F '\\' '{print $'$I'}' <<< "${BSD_Name}")
        Free_tmp=$(awk -F '\\' '{print $'$I'}' <<< "${Free}")
        Capacity_tmp=$(awk -F '\\' '{print $'$I'}' <<< "${Capacity}")
        Writable_tmp=$(awk -F '\\' '{print $'$I'}' <<< "${Writable}")
        Writable_tmp=$(awk '$1=$1' <<< $Writable_tmp)
        Title="${Volume_Name_tmp} (${Free_tmp}/${Capacity_tmp})"
        Cmd="$Title | refresh=false shell=\"`pwd`/$0\" param1=mount param2=$BSD_Name_tmp param3=\"'${Volume_Name_tmp}'\""
        if [ "$Writable_tmp" = Yes ]; then
          Cmd="⏏ $Cmd param1=umount color=green"
        else
          Cmd="$Cmd param1=mount"
          if [ ! $VAR_PASSWORD ]; then
            Cmd="$Cmd terminal=true"
          else
            Cmd="$Cmd param4=\"$VAR_PASSWORD\" terminal=false"
          fi
        fi
        echo $Cmd
    done
  fi
else
  # mount driver
  if [ "$1" = open ]; then
    mkdir -p $VAR_PATH
    open $VAR_PATH
    exit
  fi
  mount_path="$VAR_PATH/$3"
  dev="/dev/$2"
  diskutil umount $dev
  if [ "$1" = mount ]; then
    mkdir -p "${mount_path}"
    if [ ! $4 ]; then
      sudo mount -t ntfs -o rw,auto,nobrowse $dev "${mount_path}"
    else
      echo $4 | sudo -S mount -t ntfs -o rw,auto,nobrowse $dev "${mount_path}"
    fi
    INFO=$(diskutil info $dev 2>/dev/null)
    Mounted=$(awk -F: '/Mounted/{print $2}'<<<"${INFO}")
    Mounted=$(awk '$1=$1' <<< $Mounted)
    if [ "${Mounted}" = Yes ]; then
      open "${mount_path}"
      osascript -e 'display notification "Success mount '"${mount_path}"'" with title "NTFS"'
    else
      diskutil mount $dev
      osascript -e 'display dialog "Error when mount '"${mount_path}"'." with title "NTFS"'
    fi
  else
    osascript -e 'display notification "Success unmount '"${mount_path}"'" with title "NTFS"'
  fi
  script=$(awk -F '/' '{print $NF}'<<<"$0")
  open -jg 'xbar://app.xbarapp.com/refreshPlugin?path='$script
fi
