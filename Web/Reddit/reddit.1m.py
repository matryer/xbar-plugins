#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <bitbar.title>Reddit</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Parvez</bitbar.author>
# <bitbar.author.github>parvez</bitbar.author.github>
# <bitbar.desc>Displays Reddit for Mac</bitbar.desc>
# <bitbar.image>http://i.imgur.com/zUHPCvq.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/parvez/bitbar-plugins</bitbar.abouturl>
#
# by Parvez

# encoding=utf8
import sys
reload(sys)
sys.setdefaultencoding('utf8')

import json
import urllib2
reddit0 = "https://www.reddit.com"
reddit1 = [
  ["All", "https://www.reddit.com/r/all", ""],
  ["Hour", "https://www.reddit.com/r/all/top/", "?sort=top&t=hour"],
  ["News", "https://www.reddit.com/r/news+worldnews+politics", ""],
]
icons = {
  "bayarea": "🇺🇸",
  "news": "📰",
  "worldnews": "🌎"
}

print ("🐈")
print ("---")

for r1 in reddit1:
  print (r1[0])
  request = urllib2.Request(r1[1] + ".json" + r1[2], headers={
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36",
    "Pragma": "no-cache"
  })
  jsonUrl = urllib2.urlopen(request).read()
  jsonRet = json.loads(jsonUrl)
  for j in jsonRet['data']['children']:
    child = j['data']
    line = child['title']
    color = ""
    if child['quarantine'] or child['over_18']:
      line = "(NSFW) " + line
      color = " color=#EEEEEE"
    lim=75
    w=0
    l = []
    sr = child['permalink'].split("/")[2].lower()
    i = ""
    if sr in icons: i = icons[sr] + " "
    for d in line.split():
      if w + len(d) + 1 <= lim:
        l.append(d)
        w += len(d) + 1
      else:
        print ("--" + i + " ".join(l) + " | href=" + child['url'] + " trim=false size=11" + color)
        i = ""
        l = [d]
        w = len(d)
    if (len(l)):
      print ("--" + i + " ".join(l) + " | href=" + child['url'] + " trim=false size=11" + color)
    print ("--          [" + sr + "] " + "Score: " + str(child['score']) + ", Comments: " + str(child['num_comments']) + " | href=" + reddit0 + child['permalink'] + " trim=false size=9" + color)
    print ("--          | trim=false size=4")
print ("---")
print ("Refresh... | refresh=true")
