#!/usr/bin/env python3

# <xbar.title>Age Clock</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Richard Jones</xbar.author>
# <xbar.author.github>nomasprime</xbar.author.github>
# <xbar.desc>Displays your age in a clock format [y:m:d]</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>

from dateutil.relativedelta import *
from datetime import datetime

today = datetime.today()
dob = datetime(1979, 11, 8)
age = relativedelta(today, dob)

print("%02d:%02d:%02d" % (age.years, age.months, age.days))
