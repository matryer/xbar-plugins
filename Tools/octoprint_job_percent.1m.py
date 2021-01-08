#!/usr/bin/python
# <bitbar.title>Octoprint Job Percent</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tavis Gustafson</bitbar.author>
# <bitbar.author.github>tavdog</bitbar.author.github>
# <bitbar.desc>Simple python octoprint job percent display</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>

import requests
import re

# edit to your octoprint ip address:port
url = "http://192.168.1.39:5000/api/job"

# update to your api-key 
headers = {"Accept": "application/json", "X-Api-Key": "FCDD9CAD9EEA41B2AADABD51B1CC93A3"}

result = requests.get(url,headers=headers)

try:
    percent = re.search('"completion":(\d+)\.\d+', result.text).group(1)
    print(percent + "%")
except:
    print("-")
