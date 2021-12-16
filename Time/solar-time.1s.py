#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <xbar.title>Apparent Solar Time</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alexandre André</xbar.author>
# <xbar.author.github>XanderLeaDaren</xbar.author.github>
# <xbar.desc>Displays the apparent solar time. Specify your longitude in the script.</xbar.desc>
# <xbar.image>https://github.com/XanderLeaDaren/bitbar-solar-time/blob/master/bitbar_solar-time.jpg?raw=true</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/XanderLeaDaren/bitbar-solar-time</xbar.abouturl>

import datetime
from math import sin
import time

# Local time and day of the year
today = datetime.datetime.now()
day = today.timetuple().tm_yday

# Longitude on Earth (easily find it on http://ipinfo.io/json)
lg = 5.1413
pos = lg/360*24*60

# Time zone and Equation of time
tz = time.timezone
eq_time = 7.655*sin(2*(day-4))+9.873*sin(4*(day-172))

# Local solar time
sun = today - datetime.timedelta(minutes=-pos+eq_time,seconds=-tz)
print "☀️ "+sun.strftime('%H:%M:%S')
print "---"
print "Time Zone Offset: "+str(tz/3600)+" h"
print "Position Offset: %.3f" % pos+" min"
print "Equation of Time: %.3f" % -eq_time+" min"
