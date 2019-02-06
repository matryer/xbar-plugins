#!/usr/bin/env -S PATH="${PATH}:/usr/local/bin" python3

# <bitbar.title>Hacker News Headlines - Lite</bitbar.title>
# <bitbar.author>amrrs</bitbar.author>
# <bitbar.author.github>amrrs</bitbar.author.github>
# <bitbar.desc>Display Top Hacker News Headlines</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>https://raw.githubusercontent.com/amrrs/hn_headlines_bitbar/master/hn_headlines_bitbar.png</bitbar.image>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.abouturl>https://github.com/amrrs/hn_headlines_bitbar/blob/master/hn_front.120m.py</bitbar.abouturl>


from requests import get, exceptions
from sys import exit
print("HN")
try:
    content = get("https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty")
except exceptions.RequestException:    
    print('---')
    print("Internet Connection Not available")
    print("Manually refresh | refresh = true")
    exit(1)
ids = content.json()
story_base = "https://hacker-news.firebaseio.com/v0/item/"
hn_link = "https://news.ycombinator.com/item?id="
print('---')
for id in ids[:10]:
    story = get(story_base + str(id) + ".json")
    story_json = story.json()
    print(story_json["title"] + "| href = https://news.ycombinator.com/item?id=" + str(id))
