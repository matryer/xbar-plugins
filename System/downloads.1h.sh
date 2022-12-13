#!/bin/bash

DOWNLOADS_DIR="$HOME/Downloads"

# <xbar.title>Downloads</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>iosdeveloper</xbar.author>
# <xbar.author.github>iosdeveloper</xbar.author.github>
# <xbar.desc>Quick access to your recent downloads. Enter URL to download from. Specify downloads directory at the top of the file. Defaults to ~/Downloads. Contains example of how to refresh from the command line (see https://github.com/matryer/bitbar/blob/master/Docs/URLScheme.md#refreshplugin)</xbar.desc>
# <xbar.image>http://i.imgur.com/Cv4iS3d.png</xbar.image>
# <xbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/System/downloads.1h.sh</xbar.abouturl>

FILENAME=$(basename "$0")
DOWNLOADS=$(ls -t "$DOWNLOADS_DIR")

echo "Downloads"
echo "---"
echo "Download from URL… | bash='read -p Enter_URL: url && ( cd $DOWNLOADS_DIR && curl -L -# -O \$url ); open' param1=bitbar://refreshPlugin?name=$FILENAME"
echo "---"
echo "$DOWNLOADS" | awk -v DOWNLOADS_DIR="$DOWNLOADS_DIR" '{print $0" | bash=/usr/bin/open param1="DOWNLOADS_DIR"/"$0" terminal=false"}'
