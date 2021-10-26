#!/usr/bin/env python
# coding: UTF-8

# <xbar.title>It's time to</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Kouji Anzai</xbar.author>
# <xbar.author.github>kanzmrsw</xbar.author.github>
# <xbar.desc>Shows emoji means that it's time to do something.</xbar.desc>
# <xbar.image>http://i.imgur.com/qRgqIVq.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

import datetime

d = datetime.datetime.now().time()
morning = datetime.time(9,0,0)
daytime = datetime.time(17,30,0)
night = datetime.time(22,0,0)

if d < morning:
	print '🌅'
elif morning <= d < daytime:
	print '👷'
elif daytime <= d < night:
	print '🍺'
else:
	print '💤'
