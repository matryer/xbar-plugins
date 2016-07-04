#!/usr/bin/env python
# coding: UTF-8

# <bitbar.title>It's time to</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kouji Anzai</bitbar.author>
# <bitbar.author.github>kanzmrsw</bitbar.author.github>
# <bitbar.desc>Shows emoji means that it's time to do something.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/qRgqIVq.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

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
