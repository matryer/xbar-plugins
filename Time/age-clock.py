#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" python3

# <xbar.title>Age Clock</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Richard Jones</xbar.author>
# <xbar.author.github>nomasprime</xbar.author.github>
# <xbar.desc>Displays your age in a clock format [y:m:d]</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.var>number(VAR_BIRTH_YEAR=1970): Birth year</xbar.var>
# <xbar.var>number(VAR_BIRTH_MONTH=1): Birth month</xbar.var>
# <xbar.var>number(VAR_BIRTH_DAY=1): Birth day</xbar.var>

from dateutil.relativedelta import *
from datetime import datetime
import os

today = datetime.today()

dob = datetime(
    int(os.environ['VAR_BIRTH_YEAR']),
    int(os.environ['VAR_BIRTH_MONTH']),
    int(os.environ['VAR_BIRTH_DAY'])
)

age = relativedelta(today, dob)

print("%02d:%02d:%02d" % (age.years, age.months, age.days))
