#!/bin/bash

# <bitbar.title>Clipboard History</bitbar.title>
# <bitbar.author>Jason Tokoph (jason@tokoph.net)</bitbar.author>
# <bitbar.author.github>jtokoph</bitbar.author.github>
# <bitbar.desc>Tracks up to 10 clipboard items.
# <bitbar.version>1.0</bitbar.version>
# Clicking on a previous item will copy it back to the clipboard.
# Clicking "Clear history" will remove history files from the filesystem.</bitbar.desc>

# Hack for language not being set properly and unicode support
export LANG="${LANG:-en_US.UTF-8}"

tmp_dir="/tmp/bitbar-clipboard-history_$USER"

# Make sure temporary directory exists
mkdir -p "$tmp_dir" &> /dev/null

# If user clicked on a history item, copy it back to the clipboard
if [[ "$1" = "copy" ]]; then
  if [[ -e "$tmp_dir/item-$2.pb" ]]; then
    pbcopy < "$tmp_dir/item-$2.pb"
    osascript -e "display notification \"Copied to Clipboard\" with title \"BitBar Clipboard History\"" &> /dev/null
  fi
  exit
fi

# If user clicked clear, remove history items
if [[ "$1" = "clear" ]]; then
  rm -f "$tmp_dir"/item-*.pb
  osascript -e "display notification \"Cleared clipboard history\" with title \"BitBar Clipboard History\"" &> /dev/null
  exit
fi

CLIPBOARD=$(pbpaste)
# Check to see if we have text on the clipboard
if [ "$CLIPBOARD" != "" ]; then

  # Check if the current clipboard content is differnt from the previous
  echo "$CLIPBOARD" | diff "$tmp_dir/item-current.pb" - &> /dev/null

  # If so, the diff command will exit wit a non-zero status
  # shellcheck disable=SC2181
  if [ "$?" != "0" ]; then

    # Move the history backwards
    for i in {9..1}
    do
      j=$((i+1))

      if [ -e "$tmp_dir/item-$i.pb" ]; then
        cp "$tmp_dir/item-$i.pb" "$tmp_dir/item-$j.pb" &> /dev/null
      fi
    done

    # Move the previous value into the history
    cp "$tmp_dir/item-current.pb" "$tmp_dir/item-1.pb" &> /dev/null

    # Save current value
    echo "$CLIPBOARD" > "$tmp_dir/item-current.pb"
  fi
fi

# Print icon
echo '✄'
echo "---"

# Print up to 36 characters of the current clipboard
echo "Current"

content="$(pbpaste | head -c 36)"
if (( $(pbpaste | wc -c) > 36 )); then
  content="$content..."
fi
echo "${content//|/ }"

# Show history section if historical files exist
if [[ -e "$tmp_dir/item-1.pb" ]]; then

  echo "---"

  echo 'History (Click to copy)'

  # Print up to 36 characters of each historical item
  for i in {1..10}
  do
    if [ -e "$tmp_dir/item-$i.pb" ]; then
      content="$(head -c 36 "$tmp_dir/item-$i.pb")"
      if (( $(wc -c "$tmp_dir/item-$i.pb" | awk '{print $1}') > 36 )); then
        content="$content..."
      fi
      echo "${content//|/ }|bash='$0' param1=copy param2=$i refresh=true terminal=false"
    fi
  done

  echo "---"

  echo "Clear History |bash='$0' param1=clear refresh=true terminal=false "
fi
