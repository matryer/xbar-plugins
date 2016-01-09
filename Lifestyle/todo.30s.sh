#!/usr/bin/env bash
#
# Track todos in a simple ~/.todo file
#

todo_file="$HOME/.todo"
count=$(sed "/^\s*$/d" "$todo_file" | wc -l | awk '{print $1}')
echo "Todos: $count"
echo "---"
cat "$todo_file"
