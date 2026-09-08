#!/usr/bin/env bash

# <xbar.title>Cloudflare Colo</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Karolis Pocius</xbar.author>
# <xbar.author.github>kpocius</xbar.author.github>
# <xbar.desc>Shows the Cloudflare edge data center (colo) code serving your connection.</xbar.desc>
# <xbar.dependencies>bash,curl</xbar.dependencies>
# <xbar.abouturl>https://www.cloudflare.com/cdn-cgi/trace</xbar.abouturl>

export PATH='/usr/local/bin:/usr/bin:/bin:$PATH'

colo=$(curl -s --max-time 5 https://www.cloudflare.com/cdn-cgi/trace | grep '^colo=' | cut -d= -f2)

if [ -z "$colo" ]; then
  echo "colo ?"
  exit 0
fi

echo "$colo"
