#!/usr/bin/python
#
# <xbar.title>Youtube Sub gap</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Tok1</xbar.author>
# <xbar.author.github>tokfrans03</xbar.author.github>
# <xbar.desc>Displays the sub gap between Pewdiepie and T-series. Set your youtube api key in the script. You can also change the channel ids to check on other sub gaps, just make shure the bigger one is first</xbar.desc>
# <xbar.image>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</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/tokfrans03/BitBar-sub-gap</xbar.abouturl>

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