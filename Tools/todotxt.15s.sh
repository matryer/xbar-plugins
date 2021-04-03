#!/usr/bin/env bash
#
## <xbar.title>Todo.txt</xbar.title>
# <xbar.version>v1.0</xbar.version>
# # <xbar.author>Dwight Spencer</xbar.author>
# # <xbar.author.github>denzuko</xbar.author.github>
# # <xbar.desc>Todo.txt intergration with mac gui, click to do a item</xbar.desc>
# # <xbar.dependancies>todo.sh</xbar.dependancies>

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
