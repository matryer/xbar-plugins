#!/usr/bin/env sh

# <xbar.title>NTFS Mounter</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>xfangfang</xbar.author>
# <xbar.author.github>xfangfang</xbar.author.github>
# <xbar.desc>Mount NTFS Drivers</xbar.desc>
# <xbar.image>https://xfangfang.github.io/assets/img/ntfs_mounter.png</xbar.image>
# <xbar.dependencies>sh</xbar.dependencies>
# <xbar.abouturl>https://xfangfang.github.io/025</xbar.abouturl>
# <xbar.var>string(VAR_PATH="$HOME/ntfs-volume"): the driver mount path.</xbar.var>
# <xbar.var>string(VAR_PASSWORD=""): system password (optional, will be stored in plain text).</xbar.var>

# This plugin is only tested with MacOS 12.0.1
# usage:
# sh ntfs.1d.sh
# sh ntfs.1d.sh mount disk2s1 Kingston
# sh ntfs.1d.sh mount disk2s1 Kingston system_password
# sh ntfs.1d.sh umount disk2s1
# sh ntfs.1d.sh open # open volume folder

if [ ! $VAR_PATH ]; then
  VAR_PATH=$HOME/ntfs-volume
fi

if [ ! $1 ]; then
  # gui
  echo NTFS
  echo ---
  echo Refresh \| refresh=true key=CmdOrCtrl+r
  echo Open Folder \| shell=\"`pwd`/$0\" param1=open key=CmdOrCtrl+f
  echo ---
  SYSTEM_PROFILER=$(system_profiler SPStorageDataType 2>/dev/null)
  Volume_Num=$(grep "File System: NTFS"<<<"${SYSTEM_PROFILER}" | wc -l)
  ICON="iVBORw0KGgoAAAANSUhEUgAAACQAAAAkBAMAAAATLoWrAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAMFBMVEUAAABE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XlE2XnGA2icAAAAD3RSTlMAu3cRmcxmRN0i7qqIVTPwD1nfAAAAc0lEQVQoz2MYODDRAF2Esz8YXcji/0c0Eeb4//8FUIVY/////wVVSP4/EDggi3D/B4E/yEL+/8FAASHC8h8CPiGE9P9DQQJMhOk/DPyFCeX/h4MCiAjbfwT4DhGqRxL69QAsBAsCGIcwQPgPARSoKjRoAQAhwZbhTiL2ewAAAABJRU5ErkJggg=="
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
        Cmd="$Title | refresh=false shell=\"`pwd`/$0\""
        if [ "$Writable_tmp" = Yes ]; then
          Cmd="$Cmd param1=umount image=$ICON"
        else
          Cmd="$Cmd param1=mount"
        fi
        echo "$Cmd param2=$BSD_Name_tmp param3=\"'${Volume_Name_tmp}'\" param4=\"$VAR_PASSWORD\""
    done
  fi
else
  # [un]mount driver
  if [ "$1" = open ]; then
    mkdir -p $VAR_PATH
    open $VAR_PATH
    exit
  fi
  driver=`sed s/[[:space:]\']//g <<< $3`
  mount_path="$VAR_PATH/$driver-$2"
  dev="/dev/$2"
  diskutil umount $dev
  if [ "$1" = mount ]; then
    mkdir -p "${mount_path}"
    pswd=$4
    if [ ! $pswd ]; then
      pswd=`osascript -e 'text returned of (display dialog "This plugin need your password to mount the driver.\n\nYou can also set the password on the configuration page of this plugin to save time." with icon note default answer "" with title "NTFS")'`
    fi
    sudo -S mount -t ntfs -o rw,auto,nobrowse $dev "${mount_path}" <<< $pswd
    INFO=$(diskutil info $dev 2>/dev/null)
    Mounted=$(awk -F: '/Mounted/{print $2}'<<<"${INFO}")
    Mounted=$(awk '$1=$1' <<< $Mounted)
    if [ "${Mounted}" = Yes ]; then
      open "${mount_path}"
      osascript -e 'display notification "Success mount '"${mount_path}"'" with title "NTFS"'
    else
      diskutil mount $dev
      rmdir "${mount_path}"
      osascript -e 'display dialog "Error when mount '"${mount_path}"'.\n\n1. Maybe you should safely unmount this driver from a Windows computer first. \n2. Or check the password if you have already set it in xbar." with icon stop with title "NTFS"'
    fi
  else
    mount_path="$VAR_PATH/$driver"
    rmdir "${mount_path}"
    osascript -e 'display notification "Success unmount '"${mount_path}"'" with title "NTFS"'
  fi
  script=$(awk -F '/' '{print $NF}'<<<"$0")
  open -jg 'xbar://app.xbarapp.com/refreshPlugin?path='$script
fi
