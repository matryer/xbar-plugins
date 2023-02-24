#!/usr/bin/env python3

# <xbar.title>GitHub Zen</xbar.title>
# <xbar.version>v1.1.0</xbar.version>
# <xbar.author>Josh</xbar.author>
# <xbar.author.github>andjosh</xbar.author.github>
# <xbar.desc>GitHub zen in your menu bar!</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>http://i.imgur.com/U4OHxDm.png</xbar.image>

from urllib.request import urlopen

url = 'https://api.github.com/zen'
body = urlopen(url).read()
print(body.decode('utf-8'))
