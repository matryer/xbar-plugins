#!/usr/bin/env bash

# <xbar.title>Top 5 Memory-Consuming Applications</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Qiansen Y</xbar.author>
# <xbar.author.github>hypersport</xbar.author.github>
# <xbar.desc>top 5 memory-consuming applications</xbar.desc>
# <xbar.image>http://23.105.211.229/images/top5-memory-consuming.jpg</xbar.image>
# <xbar.about>top 5 memory-consuming applications</xbar.about>

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
