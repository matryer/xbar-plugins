#!/bin/bash
#
# <xbar.title>BTC Block Time</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>BitFlib</xbar.author>
# <xbar.author.github>BitFlib</xbar.author.github>
# <xbar.desc>Displays the height of the last bitcoin block aka block time (received from mempool explorer api; requires tor).</xbar.desc>
# <xbar.image>https://i.imgur.com/m16z0vd.png</xbar.image>
# <xbar.dependencies>tor</xbar.dependencies>
# <xbar.var>string(VAR_MEMPOOL_TOR_ADDRESS="http://mempoolhqx4isw62xs7abwphsq7ldayuidyx2v2oethdhhj6mlo2r6ad.onion"): mempool explorer tor address</xbar.var>

# requires installed TOR proxy service.
curl -sSL --socks5-hostname 127.0.0.1:9050 ${VAR_MEMPOOL_TOR_ADDRESS}/api/blocks/tip/height
