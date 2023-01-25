#!/bin/zsh

# <xbar.title>Mintable</xbar.title>
# <xbar.version>v2.0.0</xbar.version>
# <xbar.author>Kevin Schaich</xbar.author>
# <xbar.author.github>kevinschaich</xbar.author.github>
# <xbar.desc>Automate your personal finances – for free, with no ads, and no data collection.</xbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/9244728/86181837-c6ea7300-bafc-11ea-8442-c943eece3a03.png</xbar.image>
# <xbar.dependencies>node,mintable</xbar.dependencies>
# <xbar.abouturl>https://github.com/kevinschaich/mintable</xbar.abouturl>

export PATH="/usr/local/bin:$PATH"

output=$(/usr/local/bin/mintable fetch) || exit $?

declare output_status

if [[ $? -eq 0 ]]
then
    output_status="🍃"
else
    output_status="❌"
fi

echo $output_status
echo ---
echo "..."
echo $output | tail -n 15 | head -n 12

echo "---"
link=`echo $output | tail -n 1`
echo $link
echo "View Spreadsheet | color=green href=$link"

echo "---"
echo "Refresh Now | color=blue refresh=true"
