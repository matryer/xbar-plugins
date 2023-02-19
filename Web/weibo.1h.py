#!/usr/bin/env python3

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Hot Search of Weibo</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Garden Yuen</xbar.author>
#  <xbar.author.github>weaming</xbar.author.github>
#  <xbar.desc>Show the hot search list on China Weibo.（微博热搜）</xbar.desc>
#  <xbar.image>https://user-images.githubusercontent.com/10275711/215465324-0fc864fa-a8c7-4e9c-bc9e-00efd7ba1ab6.png</xbar.image>
#  <xbar.dependencies>python</xbar.dependencies>
#  <xbar.abouturl>https://github.com/weaming</xbar.abouturl>

from urllib.parse import quote
import json
import urllib.request

print("热搜")
print("---")
r = urllib.request.urlopen("https://weibo.com/ajax/side/hotSearch")
s = json.loads(r.read())
for x in s['data']['realtime']:
    xx = x['word']
    print(f"{xx} | href=https://s.weibo.com/weibo?q={quote(xx)}")
