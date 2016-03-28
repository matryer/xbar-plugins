#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>GitHub status</bitbar.title>
# <bitbar.version>v0.2</bitbar.version>
# <bitbar.author>Brett Jones</bitbar.author>
# <bitbar.author.github>blockloop</bitbar.author.github>
# <bitbar.image>https://cloud.githubusercontent.com/assets/3022496/12325555/a4b2bd9a-ba90-11e5-8254-9de54c2c6847.png</bitbar.image>
# <bitbar.desc>Shows the current status of status.github.com. Find out if Github is having DDOS problems which will affect pushes/pulls.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
#

import json
import time
from datetime import datetime

try:
    # For Python 3.0 and later
    from urllib.request import urlopen
except ImportError:
    # Fall back to Python 2's urllib2
    from urllib2 import urlopen

body = urlopen("https://status.github.com/api/last-message.json").read()
obj = json.loads(body.decode('utf-8'))

if obj["status"] == "good":
    # print("GH: ✔ | color=green")
    print("GH: ✔")
else:
    print("GH: 𝙭 | color=red")

print("---")
print(obj["body"] + " | href=https://status.github.com/")

# convert UTC to local
utc_date = datetime.strptime(obj["created_on"], '%Y-%m-%dT%H:%M:%SZ')
now = time.time()
offset = datetime.fromtimestamp(now) - datetime.utcfromtimestamp(now)
local_time = utc_date + offset

print("Last Change: %s" % local_time.strftime("%D %r"))
