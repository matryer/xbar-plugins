#!/usr/bin/env bash
#
# Track todos in a simple ~/.todo file
#

todo_file="$HOME/.todo"
count=$(grep -c '[^[:space:]]' "$todo_file" | awk '{print $1}')
echo "Todos: $count"
echo "---"
cat "$todo_file"
