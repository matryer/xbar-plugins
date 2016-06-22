#!/usr/bin/env bash
#
# <bitbar.author>z0mbix</bitbar.author>
# <bitbar.author.github>z0mbix</bitbar.author.github>
# <bitbar.title>Simple Todo Tracker</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
#
# Track todos in a simple ~/.todo file
#

todo_file="$HOME/.todo"
count=$(grep -c '[^[:space:]]' "$todo_file" | awk '{print $1}')
echo "Todos: $count"
echo "---"
cat "$todo_file"
