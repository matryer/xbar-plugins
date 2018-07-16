#!/usr/bin/env -S PATH="${PATH}:/usr/local/bin" python3

# <bitbar.title>Hacker News Headlines - Lite</bitbar.title>
# <bitbar.author>amrrs</bitbar.author>
# <bitbar.author.github>amrrs</bitbar.author.github>
# <bitbar.desc>Display Top Hacker News Headlines</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>https://raw.githubusercontent.com/amrrs/hn_headlines_bitbar/master/hn_headlines_bitbar.png</bitbar.image>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.abouturl>https://github.com/amrrs/hn_headlines_bitbar/blob/master/hn_front.120m.py</bitbar.abouturl>


import requests
print("HN")
#check if internet connection is available
#@yasinkuyu 08/09/2017
#source: https://gist.github.com/yasinkuyu/aa505c1f4bbb4016281d7167b8fa2fc2

def check_internet():
    url='http://www.google.com/'
    timeout=5
    try:
        _ = requests.get(url, timeout=timeout)
        return True
    except requests.ConnectionError:
        print('---')
        print("Internet Connection Not available")
    return False
if check_internet():
    content = requests.get("https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty")
    ids = content.json()
    story_base = "https://hacker-news.firebaseio.com/v0/item/"
    hn_link = "https://news.ycombinator.com/item?id="
    print('---')
    for id in ids[:10]:
        story = requests.get(story_base + str(id) + ".json")
        story_json = story.json()
        print(story_json["title"] + "| href = https://news.ycombinator.com/item?id=" + str(id))
