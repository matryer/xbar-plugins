#!/usr/bin/env bash

# <bitbar.title>Top 5 Memory-Consuming Applications</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Qiansen Y</bitbar.author>
# <bitbar.author.github>hypersport</bitbar.author.github>
# <bitbar.desc>top 5 memory-consuming applications</bitbar.desc>
# <bitbar.image>http://23.105.211.229/images/top5-memory-consuming.jpg</bitbar.image>
# <bitbar.about>top 5 memory-consuming applications</bitbar.about>

ps xmo rss=,pmem=,comm= | while read -r rss pmem comm; ((n++<5)); do
size="$((rss/1024))";
short=$((4-${#size}));
size="(${size}M)";
i=0;
while ((i++ < short)); do size=" $size"; done;
pmem="${pmem%%.*}"
if [ "$pmem" -ge 20 ]; then color=$'\e[31m';
elif [ "$pmem" -ge 10 ]; then color=$'\e[33m';
else color=$'\e[32m';
fi
echo "$color$pmem% $size $(basename "$comm")"$'\e[0m'"";
done
