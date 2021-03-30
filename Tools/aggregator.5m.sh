#!/bin/bash
#
# <xbar.title>Plugin aggregator</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>jebabin</xbar.author>
# <xbar.author.github>jebabin</xbar.author.github>
# <xbar.image>https://i.imgur.com/3TRA96Z.png</xbar.image>
# <xbar.desc>Group your xbar plugin in a single menu in the menubar. Put the aggregator script in your plugins directory, then create an aggregator folder in the plugins directory and put the plugin your want to group in that aggregator folder. Please note that the whole plugin will be refresh based on aggregator refresh time. You will also not be able to customise plugins variable via xbar GUI. You may want to manually edit the scripts. You can do nested stuff by adding an aggregator script and folder inside the aggregator folder and so on.</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.version>1.0</xbar.version>

# <xbar.var>string(VAR_NAME="xbar"): Name to display in the menubar.</xbar.var>


tmpfile=$(mktemp)
cd aggregator 
for file in *; do
	export i=0; "./$file" | grep -E -v '^---$' | while read -r line; do i=$(($i+1)); if [ "$i" -ne 1 ]; then echo -n "--" >> "$tmpfile"; fi; echo "$line" >> "$tmpfile"; done

done

echo "$VAR_NAME"
echo "---"
cat "$tmpfile"
rm "$tmpfile"