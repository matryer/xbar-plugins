#!/usr/bin/env python3

# <xbar.title>Memento Mori</xbar.title>
# <xbar.version>1.1</xbar.version>
# <xbar.author>Sufiyan Yasa</xbar.author>
# <xbar.author.github>xr1337</xbar.author.github>
# <xbar.desc>Memento Mori - Remember, thou art mortal</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>https://www.sufiyanyasa.com/img/avatar.jpg</xbar.image>

from datetime import datetime as dt
from datetime import timedelta

# example 25/12/2011
birthday = "<YOUR BIRTHDAY>"
end_age = 77

birthday_date = dt.strptime(birthday, "%d/%m/%Y")
end_date = birthday_date + timedelta(days=end_age * 365)
days_left = (end_date - dt.now()).days
print(('- {} days left -'.format(days_left)))
