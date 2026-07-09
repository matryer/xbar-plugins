#!/usr/bin/env bash

# <xbar.title>Bookmarks</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Zukky Baig</xbar.author>
# <xbar.author.github>ZukkyBaig</xbar.author.github>
# <xbar.desc>Quick-access URL bookmarks in the menubar. Click to open, add via a prompt, remove from a submenu.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/ZukkyBaig/swiftbar-bookmark-plugin/main/assets/menubar-with-bookmark.png</xbar.image>
# <xbar.dependencies>bash,osascript</xbar.dependencies>
# <xbar.abouturl>https://github.com/ZukkyBaig/swiftbar-bookmark-plugin</xbar.abouturl>

SCRIPT="$0"
BOOKMARKS_FILE="$HOME/.xbar-bookmarks.txt"

[ -f "$BOOKMARKS_FILE" ] || touch "$BOOKMARKS_FILE"

# Strip leading and trailing whitespace from a string.
trim() {
    local s="$1"
    s="${s#"${s%%[![:space:]]*}"}"
    s="${s%"${s##*[![:space:]]}"}"
    printf '%s' "$s"
}

case "$1" in
  add)
    # Prompt the user for a name and URL via AppleScript, then append to the bookmarks file.
    result=$(osascript <<'EOF' 2>/dev/null
try
  set theName to text returned of (display dialog "Bookmark name:" default answer "" with title "Add Bookmark")
  set theURL to text returned of (display dialog "URL:" default answer "https://" with title "Add Bookmark")
  return theName & "§" & theURL
on error
  return ""
end try
EOF
)
    if [ -n "$result" ]; then
      name="${result%%§*}"
      url="${result#*§}"
      name="$(trim "$name")"
      url="$(trim "$url")"
      if [ -n "$name" ] && [ -n "$url" ]; then
        case "$url" in
          http://*|https://*) ;;
          *) url="https://$url" ;;
        esac
        printf '%s | %s\n' "$name" "$url" >> "$BOOKMARKS_FILE"
      fi
    fi
    exit 0
    ;;
  remove)
    # Delete the bookmark whose name matches the given argument.
    target="$2"
    if [ -n "$target" ]; then
      tmp="$(mktemp)"
      awk -F'\\|' -v t="$target" '
        {
          name=$1
          sub(/[[:space:]]+$/, "", name)
          sub(/^[[:space:]]+/, "", name)
          if (name != t) print
        }
      ' "$BOOKMARKS_FILE" > "$tmp"
      mv "$tmp" "$BOOKMARKS_FILE"
    fi
    exit 0
    ;;
esac

# Render the menubar dropdown: bookmarks list, then add/remove/edit/refresh actions.
echo ":bookmark:"
echo "---"

if [ ! -s "$BOOKMARKS_FILE" ]; then
  echo "No bookmarks yet | color=gray"
else
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    name="$(trim "${line%%|*}")"
    url="$(trim "${line#*|}")"
    [ -z "$name" ] || [ -z "$url" ] && continue
    echo "$name | href=$url"
  done < "$BOOKMARKS_FILE"
fi

echo "---"
echo "Add bookmark… | shell=\"$SCRIPT\" param1=add refresh=true terminal=false"

if [ -s "$BOOKMARKS_FILE" ]; then
  echo "Remove bookmark"
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    name="$(trim "${line%%|*}")"
    [ -z "$name" ] && continue
    echo "-- $name | shell=\"$SCRIPT\" param1=remove param2=\"$name\" refresh=true terminal=false"
  done < "$BOOKMARKS_FILE"
fi

echo "Edit file… | shell=open param1=\"$BOOKMARKS_FILE\" terminal=false"
echo "Refresh | refresh=true"
