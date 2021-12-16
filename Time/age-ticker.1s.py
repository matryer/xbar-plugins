#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <xbar.title>Age Ticker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Gary Thung</xbar.author>
# <xbar.author.github>garythung</xbar.author.github>
# <xbar.desc>Displays your age ticking in years with decimals. Set your birthday in the script.</xbar.desc>
# <xbar.image>https://github.com/garythung/bitbar-age-ticker/blob/master/bitbar-age-ticker.gif?raw=true</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/garythung/bitbar-age-ticker</xbar.abouturl>

import datetime

# Your Birthday
birthday_year = 1970
birthday_month = 1
birthday_day = 1

birthday = datetime.datetime(birthday_year, birthday_month, birthday_day)
now = datetime.datetime.now()
seconds = (now - birthday).total_seconds()
years = seconds / 31536000

print "%.12f" % years # change the number to change precision
