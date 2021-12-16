#!/usr/bin/env bash
#
# <xbar.author>z0mbix</xbar.author>
# <xbar.author.github>z0mbix</xbar.author.github>
# <xbar.title>Simple Todo Tracker</xbar.title>
# <xbar.version>1.0</xbar.version>
#
# Track todos in a simple ~/.todo file
#

todo_file="$HOME/.todo"
count=$(grep -c '[^[:space:]]' "$todo_file" | awk '{print $1}')
echo "Todos: $count"
echo "---"
cat "$todo_file"
