#!/usr/bin/python
# coding=utf-8
#
# <xbar.title>Faith in humanity score</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>impshum</xbar.author>
# <xbar.author.github>impshum</xbar.author.github>
# <xbar.desc>Displays current score from https://faithinhumanityscore.com</xbar.desc>
#
# by impshum

from requests import get

data = get('https://faithinhumanityscore.com/api.php').json()
score = data['score']
print(score)
