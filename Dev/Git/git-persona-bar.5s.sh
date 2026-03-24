#!/usr/bin/env bash

# <swiftbar.title>Git Persona Bar</swiftbar.title>
# <swiftbar.version>v1.0.0</swiftbar.version>
# <swiftbar.author>Iryna Arkhypchenko + OSS contributors</swiftbar.author>
# <swiftbar.desc>Switch Git + SSH personas from your macOS menu bar</swiftbar.desc>
# <swiftbar.dependencies>bash,python3,git,ssh-add</swiftbar.dependencies>

# xbar metadata (for directory indexing compatibility)
# <xbar.title>Git Persona Bar</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Iryna Arkhypchenko</xbar.author>
# <xbar.author.github>IryArkhy</xbar.author.github>
# <xbar.desc>Switch up to 5 Git + SSH personas from your macOS menu bar.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/IryArkhy/git-persona-bar/main/docs/images/swiftbar-menu.png</xbar.image>
# <xbar.dependencies>bash,python3,git,ssh-add</xbar.dependencies>
# <xbar.abouturl>https://github.com/IryArkhy/git-persona-bar</xbar.abouturl>

set -u

MAX_PROFILES=5
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/git-persona-bar"
CONFIG_FILE="$CONFIG_DIR/profiles.json"
STATE_FILE="$CONFIG_DIR/state.json"
SSH_CONFIG_FILE="$HOME/.ssh/config"

resolve_script_dir() {
  local source="$1"
  while [ -L "$source" ]; do
    local dir
    dir="$(cd -P "$(dirname "$source")" && pwd)"
    source="$(readlink "$source")"
    [[ "$source" != /* ]] && source="$dir/$source"
  done
  cd -P "$(dirname "$source")" && pwd
}

SCRIPT_DIR="$(resolve_script_dir "$0")"

ensure_config_exists() {
  mkdir -p "$CONFIG_DIR"
  if [ ! -f "$CONFIG_FILE" ]; then
    if [ -f "$SCRIPT_DIR/config/profiles.example.json" ]; then
      cp "$SCRIPT_DIR/config/profiles.example.json" "$CONFIG_FILE"
    else
      cat > "$CONFIG_FILE" <<'JSON'
{
  "profiles": []
}
JSON
    fi
  fi
}

notify() {
  local title="$1"
  local message="$2"
  osascript -e "display notification \"$message\" with title \"$title\"" >/dev/null 2>&1 || true
}

expand_path() {
  python3 - "$1" <<'PY'
import os, sys
print(os.path.expanduser(sys.argv[1]))
PY
}

validate_config() {
  python3 - "$CONFIG_FILE" "$MAX_PROFILES" <<'PY'
import json, sys

config_file = sys.argv[1]
max_profiles = int(sys.argv[2])

with open(config_file, 'r', encoding='utf-8') as f:
    data = json.load(f)

profiles = data.get('profiles', [])
if not isinstance(profiles, list):
    print('Invalid: profiles must be a list')
    sys.exit(2)

if len(profiles) == 0:
    print('Config is valid, but has 0 profiles')
    sys.exit(0)

if len(profiles) > max_profiles:
    print(f'Invalid: found {len(profiles)} profiles, max is {max_profiles}')
    sys.exit(2)

seen = set()
for i, p in enumerate(profiles):
    pid = p.get('id', '').strip()
    if not pid:
        print(f'Invalid: profiles[{i}] is missing id')
        sys.exit(2)
    if pid in seen:
        print(f'Invalid: duplicate profile id "{pid}"')
        sys.exit(2)
    seen.add(pid)

    git = p.get('git', {})
    ssh = p.get('ssh', {})

    if not git.get('name') or not git.get('email'):
        print(f'Invalid: profile "{pid}" is missing git.name or git.email')
        sys.exit(2)

    if not ssh.get('host') or not ssh.get('identity_file'):
        print(f'Invalid: profile "{pid}" is missing ssh.host or ssh.identity_file')
        sys.exit(2)

print(f'Valid config: {len(profiles)} profile(s)')
PY
}

load_profiles() {
  PROFILE_COUNT=0
  PROFILE_IDS=()
  PROFILE_LABELS=()
  PROFILE_ICONS=()
  PROFILE_GIT_NAMES=()
  PROFILE_GIT_EMAILS=()
  PROFILE_SSH_HOSTS=()
  PROFILE_SSH_KEYS=()

  local rows
  rows="$(python3 - "$CONFIG_FILE" <<'PY'
import json, sys

with open(sys.argv[1], 'r', encoding='utf-8') as f:
    data = json.load(f)

for p in data.get('profiles', []):
    def clean(v):
        return str(v or '').replace('\t', ' ').replace('\n', ' ').strip()

    pid = clean(p.get('id'))
    label = clean(p.get('label') or pid)
    icon = clean(p.get('icon') or '👤')

    git = p.get('git', {})
    ssh = p.get('ssh', {})

    gname = clean(git.get('name'))
    gemail = clean(git.get('email'))
    shost = clean(ssh.get('host'))
    skey = clean(ssh.get('identity_file'))

    if pid:
        print('\t'.join([pid, label, icon, gname, gemail, shost, skey]))
PY
)"

  while IFS=$'\t' read -r pid label icon gname gemail shost skey; do
    [ -z "${pid:-}" ] && continue
    PROFILE_IDS+=("$pid")
    PROFILE_LABELS+=("$label")
    PROFILE_ICONS+=("$icon")
    PROFILE_GIT_NAMES+=("$gname")
    PROFILE_GIT_EMAILS+=("$gemail")
    PROFILE_SSH_HOSTS+=("$shost")
    PROFILE_SSH_KEYS+=("$skey")
    PROFILE_COUNT=$((PROFILE_COUNT + 1))
  done <<< "$rows"
}

get_state_active_id() {
  if [ ! -f "$STATE_FILE" ]; then
    return 1
  fi

  python3 - "$STATE_FILE" <<'PY'
import json, sys
try:
    with open(sys.argv[1], 'r', encoding='utf-8') as f:
        data = json.load(f)
    print((data.get('active_profile_id') or '').strip())
except Exception:
    print('')
PY
}

write_state_active_id() {
  local active_id="$1"
  mkdir -p "$CONFIG_DIR"
  python3 - "$STATE_FILE" "$active_id" <<'PY'
import json, sys
state_file, active_id = sys.argv[1], sys.argv[2]
with open(state_file, 'w', encoding='utf-8') as f:
    json.dump({'active_profile_id': active_id}, f, indent=2)
    f.write('\n')
PY
}

index_for_profile_id() {
  local requested_id="$1"
  local i=0
  while [ "$i" -lt "$PROFILE_COUNT" ]; do
    if [ "${PROFILE_IDS[$i]}" = "$requested_id" ]; then
      echo "$i"
      return 0
    fi
    i=$((i + 1))
  done
  return 1
}

detect_active_profile_id() {
  local state_id
  state_id="$(get_state_active_id 2>/dev/null || true)"
  if [ -n "$state_id" ]; then
    if index_for_profile_id "$state_id" >/dev/null 2>&1; then
      echo "$state_id"
      return 0
    fi
  fi

  local current_email
  current_email="$(git config --global user.email 2>/dev/null || true)"

  local i=0
  while [ "$i" -lt "$PROFILE_COUNT" ]; do
    if [ "${PROFILE_GIT_EMAILS[$i]}" = "$current_email" ]; then
      echo "${PROFILE_IDS[$i]}"
      return 0
    fi
    i=$((i + 1))
  done

  echo ""
}

upsert_ssh_host_block() {
  local host="$1"
  local identity="$2"

  mkdir -p "$(dirname "$SSH_CONFIG_FILE")"
  [ -f "$SSH_CONFIG_FILE" ] || touch "$SSH_CONFIG_FILE"

  python3 - "$SSH_CONFIG_FILE" "$host" "$identity" <<'PY'
import sys
from pathlib import Path

config_path = Path(sys.argv[1]).expanduser()
host = sys.argv[2]
identity = sys.argv[3]

start = f"# >>> git-persona-bar:{host} >>>"
end = f"# <<< git-persona-bar:{host} <<<"

text = config_path.read_text(encoding='utf-8') if config_path.exists() else ''
lines = text.splitlines()

s_idx = e_idx = None
for i, line in enumerate(lines):
    if line.strip() == start:
        s_idx = i
    if line.strip() == end and s_idx is not None and i > s_idx:
        e_idx = i
        break

managed_block = [
    start,
    f"Host {host}",
    "  AddKeysToAgent yes",
    "  UseKeychain yes",
    "  IdentitiesOnly yes",
    f"  IdentityFile {identity}",
    end,
]

# Remove old managed block (if any), then always place the managed block at the top
# so identity-related settings take precedence over later Host entries.
if s_idx is not None and e_idx is not None:
    clean_lines = lines[:s_idx] + lines[e_idx+1:]
else:
    clean_lines = lines[:]

while clean_lines and clean_lines[0].strip() == '':
    clean_lines.pop(0)

new_lines = managed_block[:]
if clean_lines:
    new_lines += [''] + clean_lines

config_path.write_text('\n'.join(new_lines) + '\n', encoding='utf-8')
PY
}

switch_profile() {
  local requested_id="$1"

  local idx
  if ! idx="$(index_for_profile_id "$requested_id")"; then
    notify "Git Persona Bar" "Profile '$requested_id' not found"
    return 1
  fi

  local label="${PROFILE_LABELS[$idx]}"
  local git_name="${PROFILE_GIT_NAMES[$idx]}"
  local git_email="${PROFILE_GIT_EMAILS[$idx]}"
  local ssh_host="${PROFILE_SSH_HOSTS[$idx]}"
  local ssh_key="${PROFILE_SSH_KEYS[$idx]}"

  if [ -z "$git_name" ] || [ -z "$git_email" ] || [ -z "$ssh_host" ] || [ -z "$ssh_key" ]; then
    notify "Git Persona Bar" "Profile '$requested_id' is missing required fields"
    return 1
  fi

  local expanded_key
  expanded_key="$(expand_path "$ssh_key")"

  if [ ! -f "$expanded_key" ]; then
    notify "Git Persona Bar" "SSH key not found: $ssh_key"
    return 1
  fi

  git config --global user.name "$git_name"
  git config --global user.email "$git_email"

  if [ -f "$SSH_CONFIG_FILE" ]; then
    cp "$SSH_CONFIG_FILE" "$SSH_CONFIG_FILE.gpb.bak.$(date +%Y%m%d%H%M%S)" 2>/dev/null || true
  fi

  upsert_ssh_host_block "$ssh_host" "$ssh_key"

  ssh-add -D >/dev/null 2>&1 || true
  ssh-add "$expanded_key" >/dev/null 2>&1 || true

  write_state_active_id "$requested_id"
  notify "Git Persona Bar" "Switched to $label"
}

print_menu() {
  local active_id="$1"
  local active_idx=""
  local active_label="Unknown"
  local active_icon="🔀"

  if [ -n "$active_id" ]; then
    active_idx="$(index_for_profile_id "$active_id" 2>/dev/null || true)"
    if [ -n "$active_idx" ]; then
      active_label="${PROFILE_LABELS[$active_idx]}"
      active_icon="${PROFILE_ICONS[$active_idx]}"
    fi
  fi

  local current_name current_email
  current_name="$(git config --global user.name 2>/dev/null || true)"
  current_email="$(git config --global user.email 2>/dev/null || true)"

  echo "$active_icon"
  echo "---"
  echo "Git Persona Bar"
  echo "Current: $active_label"
  echo "$current_name"
  echo "$current_email"
  echo "---"

  local i=0
  while [ "$i" -lt "$PROFILE_COUNT" ]; do
    local pid="${PROFILE_IDS[$i]}"
    local label="${PROFILE_LABELS[$i]}"
    local icon="${PROFILE_ICONS[$i]}"

    if [ "$pid" = "$active_id" ]; then
      echo "✓ $icon $label (active)"
    else
      echo "Switch to $icon $label | bash='$0' param1=switch param2=$pid terminal=false refresh=true"
    fi
    i=$((i + 1))
  done

  echo "---"
  echo "Open Config File | bash='open' param1='$CONFIG_FILE' terminal=false"

  if [ -x "$SCRIPT_DIR/scripts/add-profile.sh" ]; then
    echo "Add Profile (wizard) | bash='$SCRIPT_DIR/scripts/add-profile.sh' terminal=true refresh=true"
  fi

  if [ -x "$SCRIPT_DIR/scripts/install.sh" ]; then
    echo "Install / Relink Plugin | bash='$SCRIPT_DIR/scripts/install.sh' terminal=true refresh=true"
  fi

  echo "Validate Config | bash='$0' param1=validate terminal=true refresh=true"
  echo "Refresh | refresh=true"
}

main() {
  ensure_config_exists

  if ! validate_config >/dev/null 2>&1; then
    echo "⚠️"
    echo "---"
    echo "Invalid config at: $CONFIG_FILE"
    echo "Open Config File | bash='open' param1='$CONFIG_FILE' terminal=false"
    echo "Validate Config | bash='$0' param1=validate terminal=true refresh=true"
    exit 0
  fi

  load_profiles

  if [ "$PROFILE_COUNT" -eq 0 ]; then
    echo "➕"
    echo "---"
    echo "No profiles configured"
    echo "Open Config File | bash='open' param1='$CONFIG_FILE' terminal=false"
    if [ -x "$SCRIPT_DIR/scripts/add-profile.sh" ]; then
      echo "Add First Profile (wizard) | bash='$SCRIPT_DIR/scripts/add-profile.sh' terminal=true refresh=true"
    fi
    exit 0
  fi

  if [ "$PROFILE_COUNT" -gt "$MAX_PROFILES" ]; then
    echo "⚠️"
    echo "---"
    echo "Too many profiles: $PROFILE_COUNT (max $MAX_PROFILES)"
    echo "Open Config File | bash='open' param1='$CONFIG_FILE' terminal=false"
    exit 0
  fi

  case "${1:-}" in
    switch)
      if [ -z "${2:-}" ]; then
        notify "Git Persona Bar" "Missing profile id"
        exit 1
      fi
      switch_profile "$2"
      exit $?
      ;;
    validate)
      validate_config
      exit $?
      ;;
  esac

  local active_id
  active_id="$(detect_active_profile_id)"
  print_menu "$active_id"
}

main "$@"
