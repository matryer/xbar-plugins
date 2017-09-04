#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <bitbar.title>Age Ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Gary Thung</bitbar.author>
# <bitbar.author.github>garythung</bitbar.author.github>
# <bitbar.desc>Displays your age ticking in years with decimals. Set your birthday in the script.</bitbar.desc>
# <bitbar.image>https://github.com/garythung/bitbar-age-ticker/blob/master/bitbar-age-ticker.gif?raw=true</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/garythung/bitbar-age-ticker</bitbar.abouturl>
#
# If you feel so inclined, donations are appreciated!
# BTC: 15vUa9DPoh3bFBupwy6Sc5K1L7LSPxax85
# LTC: LW33s9vNFGUAsCnHcSystvV4VDY6nKfhEa
# ETH: 0x9dd29dc6877302C6D95662fB456A65BF64191e2b

import datetime

# Your Birthday
birthday_year = 1970
birthday_month = 1
birthday_day = 1

birthday = datetime.datetime(birthday_year, birthday_month, birthday_day)
now = datetime.datetime.now()
seconds = (now - birthday).total_seconds()
years = seconds / 31536000

print "%.9f" % years # change the number to change precision
