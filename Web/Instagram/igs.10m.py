#!/usr/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Instagram Stats</bitbar.title>
# <bitbar.version>v0.1.0</bitbar.version>
# <bitbar.author>impshum</bitbar.author>
# <bitbar.author.github>impshum</bitbar.author.github>
# <bitbar.desc>Displays followers & friends stats</bitbar.desc>
# <bitbar.image>https://i.imgur.com/FafFVUV.png</bitbar.image>

import json, urllib2, requests, base64
from PIL import Image
from io import BytesIO

user = "putln2" #Instagram username without the @ symbol
last_post_count = 4 #No more than 12 recent posts can be loaded

try:
    response = urllib2.urlopen("https://instagram.com/"+user)
except urllib2.HTTPError as err:
    print("HTTP Error("+str(err.code)+"), service unreachable")
page_source = response.read()

text = page_source

this = {'thia': thia, 'that': that}

finder_text_start = ('<script type="text/javascript">'
                     'window._sharedData = ')
finder_text_start_len = len(finder_text_start)-1
finder_text_end = ';</script>'

all_data_start = text.find(finder_text_start)
all_data_end = text.find(finder_text_end, all_data_start + 1)
json_str = text[(all_data_start + finder_text_start_len + 1) \
               : all_data_end]
user_info = json.loads(json_str)
follower_count = user_info['entry_data']['ProfilePage'][0]['graphql']['user']["edge_followed_by"]['count']
following_count = user_info['entry_data']['ProfilePage'][0]['graphql']['user']["edge_follow"]['count']

print("IG-{}/{}".format(follower_count, following_count))
