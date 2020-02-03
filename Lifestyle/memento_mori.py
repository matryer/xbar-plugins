#!/usr/bin/env python

# -*- coding: utf-8 -*-
# <bitbar.title>Memento Mori</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Sufiyan Yasa</bitbar.author>
# <bitbar.author.github>xr1337</bitbar.author.github>
# <bitbar.desc>Memento Mori - Remember, thou art mortal</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>https://www.sufiyanyasa.com/img/avatar.jpg</bitbar.image>


from datetime import datetime as dt
from datetime import timedelta

# example 25/12/2011
birthday = "<YOUR BIRTHDAY>"
end_age = 77

birthday_date = dt.strptime(birthday, "%d/%m/%Y")
end_date = birthday_date + timedelta(days=end_age * 365)
days_left = (end_date - dt.now()).days
print('- {} days left -'.format(days_left))


