#!/usr/bin/env bash
#
## <bitbar.title>Todo.txt</bitbar.title>
## <bitbar.version>v1.0</bitbar.version>
## <bitbar.author>Dwight Spencer</bitbar.author>
## <bitbar.author.github>denzuko</bitbar.author.github>
## <bitbar.desc>Todo.txt intergration with mac gui, click to do a item</bitbar.desc>
## <bitbar.dependancies>todo.sh</bitbar.dependancies>

# Dependancies:
#   todo.sh (brew install todo.sh)

export PATH="$PATH:/usr/local/bin"

echo "🗒"
echo "---"

list_items() {
    todo.sh ls  | awk 'NR>1 {print buf}{buf = $0}' | sed '/--/d' | awk ' { print $0,"| bash=/usr/local/bin/todo.sh param1=do param2="$1" terminal=false refresh=true" }'

}

list_items
