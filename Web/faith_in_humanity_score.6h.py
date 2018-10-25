#!/usr/bin/python
# coding=utf-8
#
# <bitbar.title>Faith in humanity score</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>impshum</bitbar.author>
# <bitbar.author.github>impshum</bitbar.author.github>
# <bitbar.desc>Displays current score from https://faithinhumanityscore.com</bitbar.desc>
#
# by impshum

from requests import get

data = get('https://faithinhumanityscore.com/api.php').json()
score = data['score']
print(score)
