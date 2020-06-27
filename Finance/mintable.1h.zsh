#!/bin/zsh

# <bitbar.title>Mintable</bitbar.title>
# <bitbar.version>v2.0.0</bitbar.version>
# <bitbar.author>Kevin Schaich</bitbar.author>
# <bitbar.author.github>kevinschaich</bitbar.author.github>
# <bitbar.desc>Automate your personal finances – for free, with no ads, and no data collection.</bitbar.desc>
# <bitbar.image>https://github.com/kevinschaich/mintable/raw/release/2.0.0/docs/logo.png</bitbar.image>
# <bitbar.dependencies>node,mintable</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/kevinschaich/mintable</bitbar.abouturl>

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
echo
echo $output
