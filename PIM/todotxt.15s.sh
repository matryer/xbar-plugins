#!/usr/bin/env bash
#
## <bitbar.title>Todo.txt</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# # <bitbar.author>Dwight Spencer</bitbar.author>
# # <bitbar.author.github>denzuko</bitbar.author.github>
# # <bitbar.desc>Todo.txt intergration with mac gui, click to do a item</bitbar.desc>
# # <bitbar.dependancies>todo.sh</bitbar.dependancies>

# Dependancies:
#   todo.sh (brew install todo.sh)

export PATH="$PATH:/usr/local/bin"

list_items() {
    todo.sh ls  | awk 'NR>1 {print buf}{buf = $0}' | sed '/--/d' | awk ' { print $0,"| bash=/usr/local/bin/todo.sh param1=do param2="$1" terminal=false refresh=true" }'
}

add_item() {
    todo.sh add "$(osascript -e 'Tell application "System Events" to display dialog "New task:" default answer ""' -e 'text returned of result' 2>/dev/null)" >/dev/null
}

if [ "${1}" == "add" ]; then
    add_item
else
    echo "🗒"
    echo "---"
    list_items
    echo "---"
    echo "Add task | bash=$0 param1=add"
fi
