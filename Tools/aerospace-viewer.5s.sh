#!/bin/bash

# <xbar.title>Aerospace Viewer</xbar.title>
# <xbar.author>Robert Thomas</xbar.author>
# <xbar.author.github>wolveix</xbar.author.github>
# <xbar.version>v1.0</xbar.version>
# <xbar.desc>Shows a list of workspaces and their apps, with clickable options to switch focus.</xbar.desc>
# <xbar.dependencies>aerospace</xbar.dependencies>

# Use absolute paths for Aerospace
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"
AEROSPACE=$(which aerospace)

# Get all windows (each line formatted as: workspace: app-name)
ALL_WINDOWS=$($AEROSPACE list-windows --all --format '%{workspace}: %{app-name}')

# Get the currently focused workspace (remove whitespace)
FOCUSED_WS=$($AEROSPACE list-workspaces --focused 2>/dev/null | tr -d ' ')

# Create a temporary directory to store workspace files
WORKSPACE_TEMP_DIR=$(mktemp -d)

# Process each line from the output
echo "$ALL_WINDOWS" | while IFS= read -r line; do
  [ -z "$line" ] && continue

  # Parse the workspace and app
  ws=$(echo "$line" | cut -d':' -f1 | tr -d ' ')
  app=$(echo "$line" | cut -d':' -f2- | sed 's/^ //')

  # Use a file named after the workspace in our temp directory
  ws_file="$WORKSPACE_TEMP_DIR/$ws"
  [ ! -f "$ws_file" ] && touch "$ws_file"

  # Append the app if it's not already present
  if ! grep -qxF "$app" "$ws_file"; then
    echo "$app" >> "$ws_file"
  fi
done

# Build the menu bar text by listing workspace files (sorted lexicographically)
MENU_TEXT=""
for ws in $(ls "$WORKSPACE_TEMP_DIR" | sort); do
  if [ -s "$WORKSPACE_TEMP_DIR/$ws" ]; then
    [ -n "$MENU_TEXT" ] && MENU_TEXT="$MENU_TEXT "
    if [ "$ws" = "$FOCUSED_WS" ]; then
      MENU_TEXT="$MENU_TEXT [$ws]"
    else
      MENU_TEXT="$MENU_TEXT $ws"
    fi
  fi
done

# Output the menu bar text
echo "$MENU_TEXT"
echo "---"

# Build the dropdown menu: list each workspace and its apps
find "$WORKSPACE_TEMP_DIR" -mindepth 1 -maxdepth 1 -print0 | sort -z | while IFS= read -r -d '' ws_full; do
  ws=$(basename "$ws_full")
  # Make the workspace header clickable to switch focus.
  if [ "$ws" = "$FOCUSED_WS" ]; then
    echo "Workspace [$ws] | bash=$AEROSPACE param1=workspace param2=$ws terminal=false refresh=true"
  else
    echo "Workspace $ws | bash=$AEROSPACE param1=workspace param2=$ws terminal=false refresh=true"
  fi

  # List each app in that workspace clickable to focus the app.
  if [ -s "$WORKSPACE_TEMP_DIR/$ws" ]; then
    while IFS= read -r app; do
      echo "-- $app | bash=/usr/bin/open param1=-a param2=\"$app\" terminal=false refresh=true"
    done < "$WORKSPACE_TEMP_DIR/$ws"
  else
    echo "-- (no apps)"
  fi
  echo "---"
done

# Clean up temporary directory
rm -r "$WORKSPACE_TEMP_DIR"