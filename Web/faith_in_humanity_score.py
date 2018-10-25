#!/usr/bin/python
# coding=utf-8
#
# <bitbar.title>Faith in humanity score</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>impshum</bitbar.author>
# <bitbar.author.github>impshum</bitbar.author.github>
# <bitbar.desc>Displays current score from https://faithinhumanityscore.com</bitbar.desc>
# <bitbar.image>https://i.imgur.com/Zpf5Rf3.png</bitbar.image>
#
# by impshum
# suggested .15m

from requests import get

data = get('https://faithinhumanityscore.com/api.php').json()
score = data['score']
print(score)
