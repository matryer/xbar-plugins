#!/bin/zsh -f
# <xbar.title>Caffeinate Timer</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>bricklen</xbar.author>
# <xbar.author.github>bricklen</xbar.author.github>
# <xbar.desc>Keep your Mac awake for a set time with caffeinate. A coffee cup in the menu bar is grey when idle and lit when active, with an HH:MM countdown to when it expires. Also lists other apps that are holding the Mac awake.</xbar.desc>
# <xbar.dependencies>zsh,caffeinate,pmset</xbar.dependencies>
# <xbar.image>https://raw.githubusercontent.com/matryer/xbar-plugins/main/System/caffeinate-timer/caffeinate_menu.png</xbar.image>
# <xbar.abouturl>https://github.com/matryer/xbar-plugins/tree/main/System/caffeinate-timer</xbar.abouturl>
# <xbar.var>string(IDLE_COLOR="#A8A8A8"): Cup colour when idle.</xbar.var>
# <xbar.var>string(ACTIVE_COLOR="#E8A33D"): Cup colour while keeping the Mac awake.</xbar.var>
# <xbar.var>boolean(SHOW_OTHERS=true): List other apps that are preventing sleep.</xbar.var>
# <xbar.var>boolean(NOTIFY=true): Post a notification when a timer runs out.</xbar.var>
#
# SwiftBar: run the script directly (no bash wrapper) and hide menu items it
# does not need.
# <swiftbar.runInBash>false</swiftbar.runInBash>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>true</swiftbar.hideLastUpdated>
#
# How it stays cheap: `zsh -f` skips every startup file, the clock comes from
# zsh/datetime instead of `date`, liveness is a `kill -0` builtin instead of
# pgrep, and parsing is done in zsh instead of piping through awk/tr. An idle
# refresh costs one fork (pmset, only when SHOW_OTHERS is on).
#
# It only ever stops the caffeinate it started itself (tracked by PID), so a
# caffeinate you or another tool started in a terminal is left alone.

emulate -L zsh
export LC_ALL=en_US.UTF-8   # menu apps may launch us with no locale; names below can be UTF-8
setopt pipe_fail extended_glob
zmodload zsh/datetime

: ${CAFFEINATE_BIN:=/usr/bin/caffeinate}
: ${IDLE_COLOR:=#A8A8A8}
: ${ACTIVE_COLOR:=#E8A33D}
: ${SHOW_OTHERS:=true}
: ${NOTIFY:=true}

data_dir=${SWIFTBAR_PLUGIN_DATA_PATH:-$HOME/Library/Application Support/caffeinate-timer}
state_file=$data_dir/state   # "<pid> <end epoch, 0 = indefinite>"
prefs_file=$data_dir/prefs   # "1" keep display on, "0" let it sleep
[[ -d $data_dir ]] || mkdir -p -- $data_dir

pid=0 end=0
if [[ -r $state_file ]]; then
  read -r pid end < $state_file
  [[ $pid == <-> && $end == <-> ]] || pid=0 end=0
fi
keep_display=1
[[ -r $prefs_file ]] && read -r keep_display < $prefs_file
[[ $keep_display == [01] ]] || keep_display=1

is_running() { (( pid > 0 )) && kill -0 $pid 2>/dev/null }

stop_ours() {
  # Check the PID still belongs to caffeinate before killing it, in case the
  # PID was recycled after caffeinate exited.
  if is_running && [[ $(ps -p $pid -o comm=) == *${CAFFEINATE_BIN:t} ]]; then
    kill $pid
  fi
  rm -f -- $state_file
  pid=0 end=0
}

# start_for <seconds>; 0 = until stopped
start_for() {
  local secs=$1 flags=-i
  (( keep_display )) && flags=-di
  stop_ours
  if (( secs > 0 )); then
    $CAFFEINATE_BIN $flags -t $secs </dev/null >/dev/null 2>&1 &!
    end=$(( EPOCHSECONDS + secs ))
  else
    $CAFFEINATE_BIN $flags </dev/null >/dev/null 2>&1 &!
    end=0
  fi
  pid=$!
  print -r -- "$pid $end" > $state_file
}

remaining() { (( end > EPOCHSECONDS )) && print $(( end - EPOCHSECONDS )) || print 0 }

ask_duration() {
  local ans h m
  ans=$(osascript \
    -e 'text returned of (display dialog "Keep awake for how long?" & return & return & "Minutes (90) or hours:minutes (1:30)" default answer "1:00" with title "Caffeinate" buttons {"Cancel", "Start"} default button "Start" cancel button "Cancel")' \
    2>/dev/null) || return 0
  ans=${ans//[[:space:]]/}
  if [[ $ans =~ '^[0-9]{1,4}$' ]]; then
    h=0 m=$(( 10#$ans ))
  elif [[ $ans =~ '^([0-9]{1,2}):([0-5][0-9])$' ]]; then
    h=$(( 10#$match[1] )) m=$(( 10#$match[2] ))
  else
    h=0 m=0
  fi
  local mins=$(( h * 60 + m ))
  if (( mins < 1 || mins > 5999 )); then
    osascript -e 'display alert "Caffeinate" message "Enter minutes (1 to 5999) or hours:minutes, for example 90 or 1:30."' >/dev/null 2>&1
    return 0
  fi
  start_for $(( mins * 60 ))
}

# ---- Menu actions --------------------------------------------------------
case ${1:-} in
  start)   [[ ${2:-} == <-> ]] && start_for $(( $2 * 60 )); exit 0 ;;
  custom)  ask_duration; exit 0 ;;
  stop)    stop_ours; exit 0 ;;
  extend)
    if [[ ${2:-} == <-> ]] && is_running && (( end > 0 )); then
      start_for $(( $(remaining) + $2 * 60 ))
    fi
    exit 0 ;;
  display)
    keep_display=$(( ! keep_display ))
    print -r -- $keep_display > $prefs_file
    # Apply to a running timer right away, keeping its expiry.
    if is_running; then
      if (( end == 0 )); then
        start_for 0
      else
        left=$(remaining)
        (( left > 0 )) && start_for $left
      fi
    fi
    exit 0 ;;
esac

# ---- Render --------------------------------------------------------------
# A recorded caffeinate that has gone away has either run out or been killed.
if (( pid > 0 )) && ! is_running; then
  if [[ $NOTIFY == true ]] && (( end > 0 && EPOCHSECONDS >= end - 5 )); then
    osascript -e 'display notification "Your Mac can sleep normally again." with title "Caffeinate timer ended"' >/dev/null 2>&1 &!
  fi
  rm -f -- $state_file
  pid=0 end=0
fi

swiftbar=${SWIFTBAR:-}
cup=$'\xe2\x98\x95\xef\xb8\x8e'   # U+2615 U+FE0E: text-style coffee cup for xbar, which has no SF Symbols
act="shell=\"$0\" terminal=false refresh=true"

active=0 label= until_txt=
if is_running; then
  active=1
  if (( end > 0 )); then
    local left=$(remaining) mins
    mins=$(( (left + 59) / 60 ))
    printf -v label '%02d:%02d' $(( mins / 60 )) $(( mins % 60 ))
    strftime -s until_txt '%-I:%M %p' $end
  fi
fi

# Menu bar
tip="Mac sleeps normally"
(( active )) && tip="Awake until ${until_txt:-you stop it}"
if [[ -n $swiftbar ]]; then
  if (( active )); then
    print -r -- "$label | sfimage=cup.and.saucer.fill sfcolor=$ACTIVE_COLOR font=Menlo size=12 tooltip=\"$tip\""
  else
    print -r -- " | sfimage=cup.and.saucer sfcolor=$IDLE_COLOR tooltip=\"$tip\""
  fi
else
  if (( active )); then
    print -r -- "$cup${label:+ $label} | color=$ACTIVE_COLOR font=Menlo size=12"
  else
    print -r -- "$cup | color=$IDLE_COLOR"
  fi
fi
print -- ---

# Status and controls
sf() { [[ -n $swiftbar ]] && print -rn -- " sfimage=$1" }
if (( active )); then
  if (( end > 0 )); then
    print -r -- "Awake until $until_txt ($label left) |$(sf clock) symbolize=false"
  else
    print -r -- "Awake until you stop it |$(sf infinity) symbolize=false"
  fi
  print -r -- "Stop now | $act param1=stop$(sf stop.circle)"
  if (( end > 0 )); then
    print -r -- "Add time |$(sf plus.circle)"
    for m t in 15 '15 minutes' 30 '30 minutes' 60 '1 hour' 120 '2 hours'; do
      print -r -- "--+ $t | $act param1=extend param2=$m"
    done
  fi
  print -- ---
  print -r -- "Start over for | size=12 color=gray"
else
  print -r -- "Sleeping normally |$(sf moon.zzz) color=gray"
  print -- ---
  print -r -- "Keep awake for | size=12 color=gray"
fi
for m t in 15 '15 minutes' 30 '30 minutes' 60 '1 hour' 120 '2 hours' 240 '4 hours' 480 '8 hours'; do
  print -r -- "$t | $act param1=start param2=$m"
done
print -r -- "Until I stop it | $act param1=start param2=0"
print -r -- "Custom... | $act param1=custom"
print -- ---
# xbar has no checkmarks, so it gets the state in the label instead.
if [[ -n $swiftbar ]]; then
  (( keep_display )) && check=" checked=true" || check=
  print -r -- "Keep display on | $act param1=display$check"
else
  (( keep_display )) && state=on || state=off
  print -r -- "Keep display on: $state | $act param1=display"
fi

# Other processes holding the Mac awake (one pmset call).
if [[ $SHOW_OTHERS == true ]]; then
  local -A seen
  local -a others
  local line name what
  for line in ${(f)"$(pmset -g assertions 2>/dev/null)"}; do
    [[ $line =~ 'pid ([0-9]+)\(([^)]*)\): \[[^]]*\] [0-9:]+ +([A-Za-z]+) +named: "([^"]*)"' ]] || continue
    case $match[3] in
      PreventUserIdleSystemSleep|PreventSystemSleep|PreventUserIdleDisplaySleep|NoIdleSleepAssertion|NoDisplaySleepAssertion) ;;
      *) continue ;;
    esac
    [[ $match[2] == powerd || $match[1] == $pid ]] && continue
    # Process and assertion names are outside text: keep them printable and
    # free of "|" so they cannot inject menu parameters.
    name=${${match[2]//[^[:print:]]/}//|/-}
    what=${${match[4]//[^[:print:]]/}//|/-}
    (( ${+seen[$name:$what]} )) && continue
    seen[$name:$what]=1
    others+=("$name: ${what[1,60]}")
  done
  if (( $#others )); then
    print -- ---
    print -r -- "Also keeping the Mac awake | size=12 color=gray"
    for line in $others; do
      print -r -- "$line | size=12 symbolize=false"
    done
  fi
fi
