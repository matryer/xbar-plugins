#!/usr/bin/env zsh
#
# <xbar.title>treeswitch</xbar.title>
# <xbar.version>v1.1.1</xbar.version>
# <xbar.author>Sindre Johannessen</xbar.author>
# <xbar.author.github>sindrej</xbar.author.github>
# <xbar.desc>Switch your local dev servers between git worktrees, right from the menu bar.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/sindrej/treeswitch/main/docs/screenshot.png</xbar.image>
# <xbar.dependencies>zsh,git,swiftbar,gh</xbar.dependencies>
# <xbar.abouturl>https://github.com/sindrej/treeswitch</xbar.abouturl>
#
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>true</swiftbar.hideLastUpdated>
# <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
# <swiftbar.hideSwiftBar>true</swiftbar.hideSwiftBar>
# <swiftbar.refreshOnOpen>true</swiftbar.refreshOnOpen>
#
# treeswitch — SwiftBar plugin AND click-action dispatcher in one file.
#
#   - Run with no args  -> render the menu bar dropdown.
#   - Run with an action -> perform it (start / stop / stopall / restart /
#     resetmain / prsync / openlog / watch / addrepo / editrepo / removerepo).
#
# SwiftBar runs this file on its refresh interval to draw the menu, and runs it
# again (with params) when you click an item.

# GUI apps get a minimal PATH, so set one that finds git/lsof + brew/uv/node.
export PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"

SELF="${0:A}"                       # absolute path to this file (resolves symlink)
DATA="$HOME/.treeswitch"
CONF="$DATA/config.zsh"
STATE="$DATA/state"
LOGS="$DATA/logs"
mkdir -p "$STATE" "$LOGS" "$DATA/cache"

[[ -f "$CONF" ]] && source "$CONF"

# ---------------------------------------------------------------------------
# small helpers
# ---------------------------------------------------------------------------

# pid(s) LISTENing on a tcp port
port_pid() { lsof -ti tcp:"$1" -sTCP:LISTEN 2>/dev/null }

# macOS notification
notify() { osascript -e "display notification \"$1\" with title \"treeswitch\"" >/dev/null 2>&1 }

# escape a string for safe embedding inside an AppleScript double-quoted literal
# (also turns real newlines into \n so multi-line messages don't break the parse)
_osa() { local s="${1//\\/\\\\}"; s="${s//\"/\\\"}"; print -r -- "${s//$'\n'/\\n}" }

# yes/no dialog — returns 0 only if the user clicks OK
confirm() {
  osascript -e "display dialog \"$(_osa "$1")\" buttons {\"Cancel\",\"OK\"} default button \"OK\" with icon caution" >/dev/null 2>&1
}
# escape a string for safe embedding inside a zsh double-quoted config value
_zq()  { local s="${1//\\/\\\\}"; print -r -- "${s//\"/\\\"}" }

# native text prompt — prints what the user typed; non-zero exit if they cancel
ask_text() {
  osascript -e "text returned of (display dialog \"$(_osa "$1")\" default answer \"$(_osa "$2")\" buttons {\"Cancel\",\"OK\"} default button \"OK\" with title \"treeswitch\")" 2>/dev/null
}
# native Finder folder picker — prints the chosen POSIX path; non-zero if cancelled
ask_folder() {
  osascript -e 'POSIX path of (choose folder with prompt "Select the git repository folder")' 2>/dev/null
}
# native alert (used for validation errors)
alert() {
  osascript -e "display alert \"$(_osa "$1")\" message \"$(_osa "$2")\"" >/dev/null 2>&1
}

# short git hint for a worktree: " ●" if dirty, " ↑n ↓n" vs upstream
wt_hint() {
  local wt="$1" hint="" ab a b
  [[ -n "$(git -C "$wt" status --porcelain 2>/dev/null | head -1)" ]] && hint+=" ●"
  ab=$(git -C "$wt" rev-list --left-right --count '@{upstream}...HEAD' 2>/dev/null)
  if [[ -n "$ab" ]]; then
    b=${ab%%[[:space:]]*}; a=${ab##*[[:space:]]}
    [[ "$a" == <-> && "$a" != 0 ]] && hint+=" ↑$a"
    [[ "$b" == <-> && "$b" != 0 ]] && hint+=" ↓$b"
  fi
  print -r -- "$hint"
}

# emit "<path>\t<branch>" per worktree of a repo
list_worktrees() {
  local repo="$1" line p
  git -C "$repo" worktree list --porcelain 2>/dev/null | while IFS= read -r line; do
    case "$line" in
      "worktree "*) p="${line#worktree }" ;;
      "branch "*)   print -r -- "${p}"$'\t'"${line#branch refs/heads/}" ;;
      "detached")   print -r -- "${p}"$'\t'"(detached)" ;;
    esac
  done
}

# default branch of a repo (origin/HEAD, e.g. "main"; falls back to main/master)
default_branch() {
  local repo="$1" d b
  d=$(git -C "$repo" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null)
  d=${d#origin/}
  if [[ -z "$d" ]]; then
    for b in main master; do
      git -C "$repo" show-ref --verify --quiet "refs/heads/$b" && { d=$b; break }
    done
  fi
  print -r -- "$d"
}

# path of the repo's primary (non-linked) working tree — git lists it first.
# This is the project's checked-out folder, where "Reset to main" puts the
# default branch (main lives here, not in a separate worktree).
primary_worktree() {
  git -C "$1" worktree list --porcelain 2>/dev/null | awk '/^worktree /{print substr($0,10); exit}'
}

# ---------------------------------------------------------------------------
# process lifecycle
# ---------------------------------------------------------------------------

# Stop a repo's server: kill the whole process group we launched (so the
# uv-run/reloader parent and any ng-serve children die too), then fall back to
# whatever still holds the port. TERM first, KILL after ~3s.
stop_server() {
  local key="$1" port="${PORT[$key]}" pgid="" lp pids i
  [[ -f "$STATE/$key.pgid" ]] && pgid="$(cat "$STATE/$key.pgid")"

  # If we have no recorded group (e.g. server was started outside the tool),
  # derive the group from whoever holds the port.
  if [[ -z "$pgid" ]]; then
    lp=$(port_pid "$port" | head -1)
    [[ -n "$lp" ]] && pgid=$(ps -o pgid= -p "$lp" 2>/dev/null | tr -d ' ')
  fi

  [[ "$pgid" == <2-> ]] && kill -TERM -"$pgid" 2>/dev/null
  pids=$(port_pid "$port"); [[ -n "$pids" ]] && kill -TERM ${=pids} 2>/dev/null

  for i in {1..15}; do
    [[ -z "$(port_pid "$port")" ]] && break
    sleep 0.2
  done

  if [[ -n "$(port_pid "$port")" ]]; then           # still up → escalate
    [[ "$pgid" == <2-> ]] && kill -9 -"$pgid" 2>/dev/null
    pids=$(port_pid "$port"); [[ -n "$pids" ]] && kill -9 ${=pids} 2>/dev/null
  fi
}

do_start() {
  local key="$1" wt="$2"
  [[ -n "${REPO[$key]}" ]] || { echo "unknown repo: $key" >&2; return 1 }
  local port="${PORT[$key]}" log="$LOGS/$key.log"

  if [[ "${CONFIRM_KILL:-0}" == "1" && -n "$(port_pid "$port")" ]]; then
    confirm "Restart ${LABEL[$key]} (:$port) from ${wt:t}? This kills the running server." || return 0
  fi

  stop_server "$key"

  local dir="$wt"
  [[ -n "${WORKDIR[$key]}" && "${WORKDIR[$key]}" != "." ]] && dir="$wt/${WORKDIR[$key]}"

  [[ -f "$log" ]] && mv -f "$log" "$log.prev"     # rotate: one fresh log per launch
  print -r -- "===== $(date '+%Y-%m-%d %H:%M:%S')  start ${key} @ ${wt} =====" >> "$log"

  local cmd="cd ${(q)dir}"
  [[ "${NPM_INSTALL[$key]}" == "1" ]] && cmd+=" && { [[ -d node_modules ]] || npm install; }"
  cmd+=" && exec ${CMD[$key]}"

  # Launch in its OWN session/process group via setsid (perl is always present
  # on macOS). The leader's PID == the new PGID, so `kill -- -<pid>` later takes
  # down the entire tree. nohup + zsh -l keeps it alive with a login PATH.
  nohup perl -e 'use POSIX qw(setsid); setsid(); exec @ARGV or die $!' /bin/zsh -lc "$cmd" >> "$log" 2>&1 &!
  local leader=$!
  print -r -- "$wt"     > "$STATE/$key.active"
  print -r -- "$leader" > "$STATE/$key.pgid"

  # detached readiness watcher → notify if the port never comes up
  nohup "$SELF" watch "$key" >/dev/null 2>&1 &!
}

do_stop() {
  local key="$1"
  if [[ "${CONFIRM_KILL:-0}" == "1" && -n "$(port_pid "${PORT[$key]}")" ]]; then
    confirm "Stop ${LABEL[$key]} (:${PORT[$key]})?" || return 0
  fi
  stop_server "$key"
  rm -f "$STATE/$key.active" "$STATE/$key.pgid"
}

do_stopall() {
  if [[ "${CONFIRM_KILL:-0}" == "1" ]]; then
    confirm "Stop ALL dev servers?" || return 0
  fi
  local key
  for key in $REPO_KEYS; do
    stop_server "$key"
    rm -f "$STATE/$key.active" "$STATE/$key.pgid"
  done
}

do_restart() {
  local key="$1" wt=""
  [[ -f "$STATE/$key.active" ]] && wt="$(cat "$STATE/$key.active")"
  [[ -n "$wt" ]] || { notify "No active worktree to restart for ${LABEL[$key]}"; return 1 }
  do_start "$key" "$wt"
}

# switch every repo's primary checkout to its default (main) branch and start it
do_resetmain() {
  if [[ "${CONFIRM_KILL:-0}" == "1" ]]; then
    confirm "Switch every repo's checkout to its default branch and restart?" || return 0
  fi
  local CONFIRM_KILL=0      # confirmed once above; don't re-prompt per repo
  local key primary def cur
  for key in $REPO_KEYS; do
    primary="$(primary_worktree "${REPO[$key]}")"
    [[ -n "$primary" ]] || primary="${REPO[$key]}"
    def="$(default_branch "${REPO[$key]}")"
    if [[ -z "$def" ]]; then
      notify "${LABEL[$key]}: couldn't determine the default branch — skipped"
      continue
    fi
    cur="$(git -C "$primary" symbolic-ref --short HEAD 2>/dev/null)"
    if [[ "$cur" != "$def" ]] && ! git -C "$primary" switch "$def" >/dev/null 2>&1; then
      notify "${LABEL[$key]}: couldn't switch ${primary:t} to ${def} — uncommitted changes, or ${def} is checked out in another worktree. Skipped."
      continue
    fi
    do_start "$key" "$primary"
  done
}

# refresh the branch->PR cache for every repo (one `gh pr list` per repo).
# Runs detached/throttled from render so the menu never blocks on the network.
do_prsync() {
  mkdir -p "$DATA/cache"
  local key repo tmp
  for key in $REPO_KEYS; do
    repo="${REPO[$key]}"
    tmp="$DATA/cache/$key.prs.tmp"
    if ( cd "$repo" 2>/dev/null && gh pr list --state open \
           --json number,headRefName,isDraft \
           --jq '.[] | [.headRefName, (.number|tostring), (.isDraft|tostring)] | @tsv' ) > "$tmp" 2>/dev/null; then
      mv -f "$tmp" "$DATA/cache/$key.prs"
    else
      rm -f "$tmp"
    fi
  done
}

# poll up to ~25s for the port; notify if it never listens (and we weren't stopped)
do_watch() {
  local key="$1" port="${PORT[$key]}" i
  for i in {1..25}; do
    [[ -f "$STATE/$key.active" ]] || return 0      # stopped/switched meanwhile
    [[ -n "$(port_pid "$port")" ]] && return 0     # it's up
    sleep 1
  done
  [[ -f "$STATE/$key.active" ]] && notify "${LABEL[$key]} didn't come up on :$port — check the log"
}

do_openlog() {
  local key="$1" log="$LOGS/$key.log"
  [[ -f "$log" ]] || : > "$log"
  exec tail -n 300 -F "$log"
}

# create an empty, well-formed config the first time we need one
ensure_config() {
  [[ -f "$CONF" ]] && return 0
  mkdir -p "$DATA"
  cat > "$CONF" <<'EOF'
# treeswitch configuration.
# Managed by the menu's "Add repo…" item — you can also edit it by hand.

typeset -gA LABEL REPO PORT CMD WORKDIR NPM_INSTALL OPEN_URL
typeset -ga REPO_KEYS

# Confirm before killing a running server (0 = off, 1 = on).
CONFIRM_KILL=0
# Show GitHub PR numbers next to worktree branches (needs `gh`; 0 = off, 1 = on).
SHOW_PRS=1
EOF
}

# visual "Add a repo" wizard: native folder picker + a few prompts, then append
# a repo block to the config. No file editing required.
do_addrepo() {
  local folder label port cmd key url npmi n=2

  folder="$(ask_folder)" || return 0
  folder="${folder%/}"
  [[ -n "$folder" ]] || return 0
  if ! git -C "$folder" rev-parse --git-dir >/dev/null 2>&1; then
    alert "Not a git repository" "$folder doesn't look like a git repo — pick the folder that contains its .git."
    return 1
  fi

  label="$(ask_text "Name for this repo (shown in the menu):" "${folder:t}")" || return 0
  [[ -n "$label" ]] || return 0
  port="$(ask_text "Dev-server port to manage (e.g. 4200):" "")" || return 0
  if [[ "$port" != <-> ]]; then
    alert "Invalid port" "\"$port\" isn't a number."
    return 1
  fi
  cmd="$(ask_text "Command that starts the dev server:" "npm start")" || return 0
  [[ -n "$cmd" ]] || return 0

  ensure_config

  # derive a unique array key (slug) from the label
  key="${label:l}"; key="${key//[^a-z0-9]/-}"
  while [[ "$key" == *--* ]]; do key="${key//--/-}"; done
  key="${key#-}"; key="${key%-}"; [[ -n "$key" ]] || key="repo"
  if [[ -n "${REPO[$key]}" ]]; then
    while [[ -n "${REPO[${key}${n}]}" ]]; do (( n++ )); done
    key="${key}${n}"
  fi

  npmi=0
  case "$cmd" in npm*|npx*|yarn*|pnpm*|"ng "*) npmi=1 ;; esac
  url="http://localhost:$port"

  {
    print -r -- ""
    print -r -- "# --- ${label} ---"
    print -r -- "REPO_KEYS+=($key)"
    print -r -- "LABEL[$key]=\"$(_zq "$label")\""
    print -r -- "REPO[$key]=\"$(_zq "$folder")\""
    print -r -- "PORT[$key]=$port"
    print -r -- "CMD[$key]=\"$(_zq "$cmd")\""
    print -r -- "WORKDIR[$key]=\".\""
    print -r -- "NPM_INSTALL[$key]=$npmi"
    print -r -- "OPEN_URL[$key]=\"$url\""
  } >> "$CONF"

  notify "Added \"$label\" — open the menu and pick a worktree to start it."
}

# regenerate config.zsh from the in-memory arrays (canonical form). Edit/Remove
# need to rewrite, not append — so the previous file is saved as config.zsh.bak.
rewrite_config() {
  ensure_config
  cp -f "$CONF" "$CONF.bak" 2>/dev/null
  local k
  {
    print -r -- "# treeswitch configuration."
    print -r -- "# Managed by the menu's Add / Edit / Remove repo items — you can also edit"
    print -r -- "# it by hand (the previous version is saved as config.zsh.bak)."
    print -r -- ""
    print -r -- "typeset -gA LABEL REPO PORT CMD WORKDIR NPM_INSTALL OPEN_URL"
    print -r -- "typeset -ga REPO_KEYS"
    print -r -- ""
    print -r -- "CONFIRM_KILL=${CONFIRM_KILL:-0}"
    print -r -- "SHOW_PRS=${SHOW_PRS:-1}"
    for k in $REPO_KEYS; do
      print -r -- ""
      print -r -- "# --- ${LABEL[$k]:-$k} ---"
      print -r -- "REPO_KEYS+=($k)"
      print -r -- "LABEL[$k]=\"$(_zq "${LABEL[$k]}")\""
      print -r -- "REPO[$k]=\"$(_zq "${REPO[$k]}")\""
      print -r -- "PORT[$k]=${PORT[$k]}"
      print -r -- "CMD[$k]=\"$(_zq "${CMD[$k]}")\""
      print -r -- "WORKDIR[$k]=\"$(_zq "${WORKDIR[$k]:-.}")\""
      print -r -- "NPM_INSTALL[$k]=${NPM_INSTALL[$k]:-0}"
      print -r -- "OPEN_URL[$k]=\"$(_zq "${OPEN_URL[$k]:-http://localhost:${PORT[$k]}}")\""
    done
  } > "$CONF"
}

# visual "Edit repo" wizard: re-prompt name / port / command, pre-filled with
# the current values. The key is kept stable (state & cache stay attached).
do_editrepo() {
  local key="$1"
  [[ -n "$key" && -n "${REPO[$key]}" ]] || { alert "Unknown repo" "There's nothing to edit."; return 1 }
  local label port cmd oldport="${PORT[$key]}" oldurl="${OPEN_URL[$key]}"

  label="$(ask_text "Name for this repo (shown in the menu):" "${LABEL[$key]}")" || return 0
  [[ -n "$label" ]] || return 0
  port="$(ask_text "Dev-server port to manage:" "$oldport")" || return 0
  if [[ "$port" != <-> ]]; then
    alert "Invalid port" "\"$port\" isn't a number."
    return 1
  fi
  cmd="$(ask_text "Command that starts the dev server:" "${CMD[$key]}")" || return 0
  [[ -n "$cmd" ]] || return 0

  LABEL[$key]="$label"
  PORT[$key]="$port"
  CMD[$key]="$cmd"
  # if the Open URL was the plain localhost default, follow the port change
  [[ "$oldurl" == "http://localhost:$oldport" ]] && OPEN_URL[$key]="http://localhost:$port"

  rewrite_config
  notify "Updated \"$label\"."
}

# visual "Remove repo": confirm, drop it from the config, tidy its state/cache.
# The actual git repo, worktrees, and any running server are left untouched.
do_removerepo() {
  local key="$1"
  [[ -n "$key" && -n "${REPO[$key]}" ]] || { alert "Unknown repo" "There's nothing to remove."; return 1 }
  local label="${LABEL[$key]:-$key}"
  confirm "Remove \"$label\" from treeswitch?

This only removes it from the menu — your repo, its worktrees, and any running server are left untouched." || return 0

  REPO_KEYS=(${REPO_KEYS:#$key})
  unset "LABEL[$key]" "REPO[$key]" "PORT[$key]" "CMD[$key]" \
        "WORKDIR[$key]" "NPM_INSTALL[$key]" "OPEN_URL[$key]"

  rewrite_config
  rm -f "$STATE/$key.active" "$STATE/$key.pgid" "$DATA/cache/$key.prs"
  notify "Removed \"$label\"."
}

# ---------------------------------------------------------------------------
# menu rendering
# ---------------------------------------------------------------------------

render_menu() {
  local rkey rcount=0
  for rkey in $REPO_KEYS; do [[ -n "$(port_pid "${PORT[$rkey]}")" ]] && (( rcount++ )); done
  # menu-bar title. SwiftBar draws an SF Symbol via `sfimage`; xbar (and any
  # other host) ignores it, so without a text glyph the icon would render blank.
  # Gate on $SWIFTBAR so the SwiftBar look is unchanged and xbar still shows "⎇".
  if [[ -n "$SWIFTBAR" ]]; then
    if (( rcount > 0 )); then
      print -r -- "${rcount} | sfimage=arrow.triangle.branch color=#3fb950"
    else
      print -r -- "| sfimage=arrow.triangle.branch"
    fi
  else
    if (( rcount > 0 )); then
      print -r -- "⎇ ${rcount} | color=#3fb950"
    else
      print -r -- "⎇"
    fi
  fi
  print -r -- "---"

  if (( ${#REPO_KEYS} == 0 )); then
    print -r -- "Welcome to treeswitch 🌳 | color=orange"
    print -r -- "No repos configured yet."
    print -r -- "➕ Add your first repo… | bash=\"$SELF\" param1=addrepo terminal=false refresh=true"
    print -r -- "---"
    print -r -- "How it works | href=https://github.com/sindrej/treeswitch"
    return
  fi

  print -r -- "↩ Reset to main | bash=\"$SELF\" param1=resetmain terminal=false refresh=true"
  print -r -- "---"

  # kick off a background PR refresh if the cache is stale (non-blocking, throttled)
  if [[ "${SHOW_PRS:-1}" == "1" ]]; then
    local stamp="$DATA/cache/.lastsync"
    if [[ ! -f "$stamp" ]] || (( $(date +%s) - $(stat -f %m "$stamp") >= 120 )); then
      touch "$stamp"
      nohup "$SELF" prsync >/dev/null 2>&1 &!
    fi
  fi

  local -A PRMAP
  local key
  for key in $REPO_KEYS; do
    local port="${PORT[$key]}" active="" pid="" runtxt="" hdr_color="" url=""
    [[ -f "$STATE/$key.active" ]] && active="$(cat "$STATE/$key.active")"
    pid="$(port_pid "$port" | head -1)"
    if [[ -n "$pid" ]]; then runtxt="● running"; hdr_color="green"
    else runtxt="○ stopped"; hdr_color="#888888"; fi

    print -r -- "${LABEL[$key]} (:${port}) ${runtxt} | color=${hdr_color}"

    [[ -z "$pid" && -n "$active" ]] && \
      print -r -- "--⚠ not running — last start may have failed | color=orange"

    PRMAP=()
    if [[ "${SHOW_PRS:-1}" == "1" && -f "$DATA/cache/$key.prs" ]]; then
      local _b="" _n="" _d=""
      while IFS=$'\t' read -r _b _n _d; do
        [[ -n "$_b" ]] || continue
        [[ "$_d" == "true" ]] && PRMAP[$_b]="#$_n draft" || PRMAP[$_b]="#$_n"
      done < "$DATA/cache/$key.prs"
    fi

    local wpath="" wbranch="" mark="" c="" hint=""
    list_worktrees "${REPO[$key]}" | while IFS=$'\t' read -r wpath wbranch; do
      mark="   "; c=""
      if [[ "$wpath" == "$active" && -n "$pid" ]]; then mark="✓ "; c="color=green"; fi
      hint="$(wt_hint "$wpath")"
      print -r -- "--${mark}${wbranch}${PRMAP[$wbranch]:+  ${PRMAP[$wbranch]}}${hint}  —  ${wpath:t} | bash=\"$SELF\" param1=start param2=${key} param3=\"${wpath}\" terminal=false refresh=true ${c}"
    done

    print -r -- "-----"
    url="${OPEN_URL[$key]:-http://localhost:$port}"
    print -r -- "--Open ${url} | href=${url}"
    if [[ -n "$pid" ]]; then
      print -r -- "--↻ Restart current | bash=\"$SELF\" param1=restart param2=${key} terminal=false refresh=true"
      print -r -- "--Stop server | bash=\"$SELF\" param1=stop param2=${key} terminal=false refresh=true color=red"
    fi
    print -r -- "--Stream log | bash=\"$SELF\" param1=openlog param2=${key} terminal=true"
    print -r -- "-----"
    print -r -- "--✎ Edit repo… | bash=\"$SELF\" param1=editrepo param2=${key} terminal=false refresh=true"
    print -r -- "--✕ Remove repo… | bash=\"$SELF\" param1=removerepo param2=${key} terminal=false refresh=true color=red"
  done

  print -r -- "---"
  (( rcount > 0 )) && \
    print -r -- "Stop all | bash=\"$SELF\" param1=stopall terminal=false refresh=true color=red"
  print -r -- "➕ Add repo… | bash=\"$SELF\" param1=addrepo terminal=false refresh=true"
  print -r -- "Refresh | refresh=true"
  print -r -- "Edit config | bash=/usr/bin/open param1=-t param2=\"$CONF\" terminal=false"
}

# ---------------------------------------------------------------------------
# dispatch — scan args for a known action keyword (index-agnostic, so it works
# regardless of whether SwiftBar passes param1 as $1 or $0)
# ---------------------------------------------------------------------------

action=""; key=""; wt=""
for a in "$@"; do
  if [[ -z "$action" ]]; then
    case "$a" in start|stop|stopall|restart|resetmain|openlog|watch|prsync|addrepo|editrepo|removerepo) action="$a" ;; esac
    continue
  fi
  if   [[ -z "$key" ]]; then key="$a"
  elif [[ -z "$wt"  ]]; then wt="$a"
  fi
done

case "$action" in
  start)   do_start "$key" "$wt" ;;
  stop)    do_stop "$key" ;;
  stopall) do_stopall ;;
  restart)   do_restart "$key" ;;
  resetmain) do_resetmain ;;
  prsync)  do_prsync ;;
  openlog) do_openlog "$key" ;;
  watch)   do_watch "$key" ;;
  addrepo)    do_addrepo ;;
  editrepo)   do_editrepo "$key" ;;
  removerepo) do_removerepo "$key" ;;
  *)       render_menu ;;
esac
