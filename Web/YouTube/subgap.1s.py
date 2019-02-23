#!/usr/bin/python
#
# <bitbar.title>Youtube Sub gap</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tok1</bitbar.author>
# <bitbar.author.github>tokfrans03</bitbar.author.github>
# <bitbar.desc>Displays the sub gap between Pewdiepie and T-series. Set your youtube api key in the script. You can also change the channel ids to check on other sub gaps, just make shure the bigger one is first</bitbar.desc>
# <bitbar.image>https://github.com/garythung/bitbar-age-ticker/blob/master/bitbar-age-ticker.gif?raw=true</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/tokfrans03/BitBar-sub-gap</bitbar.abouturl>

import requests
import json

YTchannel1="UC-lHJZR3Gqxm24_Vd_AJ5Yw" #pewdiepie
YTchannel2="UCq-Fj5jknLsUf-MWSy4_brA" #tseries
apiKey = "YOUR_API_KEY"

YTchannel1data = requests.get('https://www.googleapis.com/youtube/v3/channels?part=statistics&id=' + YTchannel1 + '&key=' + apiKey)
YTchannel2data = requests.get('https://www.googleapis.com/youtube/v3/channels?part=statistics&id=' + YTchannel2 + '&key=' + apiKey)
YTchannel1subs = json.loads(YTchannel1data.text)["items"][0]["statistics"]["subscriberCount"]
YTchannel2subs = json.loads(YTchannel2data.text)["items"][0]["statistics"]["subscriberCount"]

dif = int(YTchannel1subs) - int(YTchannel2subs)

print("{:,d}".format(int(dif)))
print("---")
print("Channel 1 | color=blue")
print("{:,d}".format(int(YTchannel1subs)))
print("---")
print("Channel 2 | color=red")
print("{:,d}".format(int(YTchannel2subs)))
print("---")
print("Gap | color=green")
print("{:,d}".format(int(dif)))