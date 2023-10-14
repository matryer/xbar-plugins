#!/bin/zsh

# Metadata allows the plugin to show up in the xbar app and website.
#
#  <xbar.title>Docker stats</xbar.title>
#  <xbar.version>v0.1</xbar.version>
#  <xbar.author>Tom Adamczewski</xbar.author>
#  <xbar.author.github>tadamcz</xbar.author.github>
#  <xbar.desc>Shows stats about currently running Docker containers</xbar.desc>
#  <xbar.image>https://images2.imgbox.com/e6/1a/3AEpT9sd_o.png</xbar.image>
#  <xbar.var>string(VAR_MENU_BAR_TEXT='🐳'): The text that will appear in the Menu Bar</xbar.var>

echo $VAR_MENU_BAR_TEXT
echo "---"
while read -r line; do
    # add `| font='MesloLGL Nerd Font Mono'` to every line
    echo "$line" " | font='MesloLGL Nerd Font Mono'"
done < <(/usr/local/bin/docker stats --no-stream)
echo "Refresh | terminal=false refresh=true"