#!/usr/bin/python
# -*- coding: utf-8 -*-
# Author: Jan M. Groß

# <bitbar.title>Instagram Stats</bitbar.title>
# <bitbar.version>v0.5.1</bitbar.version>
# <bitbar.author>Jan Groß</bitbar.author>
# <bitbar.author.github>JanGross</bitbar.author.github>
# <bitbar.desc>Displays information about an Instagram profiles followers and recent posts. Does not require any auth-token or login!</bitbar.desc>
# <bitbar.image>https://minzkraut.com/static/images/igstats_plugin.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json, urllib2, requests, base64
from PIL import Image
from io import BytesIO

user = "minzkraut" #Instagram username without the @ symbol
last_post_count = 4 #No more than 12 recent posts can be loaded

try:
    response = urllib2.urlopen("https://instagram.com/"+user)
except urllib2.HTTPError as err:
    print("HTTP Error("+str(err.code)+"), service unreachable")
page_source = response.read()

text = page_source

finder_text_start = ('<script type="text/javascript">'
                     'window._sharedData = ')
finder_text_start_len = len(finder_text_start)-1
finder_text_end = ';</script>'

all_data_start = text.find(finder_text_start)
all_data_end = text.find(finder_text_end, all_data_start + 1)
json_str = text[(all_data_start + finder_text_start_len + 1) \
               : all_data_end]
user_info = json.loads(json_str)
follower_count = user_info['entry_data']['ProfilePage'][0]['user']["followed_by"]['count']
following_count = user_info['entry_data']['ProfilePage'][0]['user']["follows"]['count']
full_name = user_info['entry_data']['ProfilePage'][0]['user']['full_name'].encode('utf8')
media_count = user_info['entry_data']['ProfilePage'][0]['user']["media"]['count']


last_medias = []
for number in range(0,last_post_count):
    try:
        last_image_url = user_info['entry_data']['ProfilePage'][0]['user']["media"]['nodes'][number]['thumbnail_src']
        last_image_likes = user_info['entry_data']['ProfilePage'][0]['user']["media"]['nodes'][number]['likes']['count']
        last_image_caption = user_info['entry_data']['ProfilePage'][0]['user']["media"]['nodes'][number]['caption']

        last_image_comments = user_info['entry_data']['ProfilePage'][0]['user']["media"]['nodes'][number]['comments']['count']

        response = requests.get(last_image_url)
        img = Image.open(BytesIO(response.content))
        img.thumbnail((50,50), Image.ANTIALIAS)
        buffered = BytesIO()
        img.save(buffered, format="JPEG")
        last_image_b64 = base64.b64encode(buffered.getvalue())

        last_medias.append({'last_image_url' : last_image_url,'last_image_likes' : last_image_likes, 'last_image_comments' : last_image_comments,'last_image_caption' : last_image_caption ,'last_image_b64' : last_image_b64})
    except:
        pass



bio = user_info ['entry_data']['ProfilePage'][0]['user']['biography']

uimage_url = user_info['entry_data']['ProfilePage'][0]['user']['profile_pic_url']

response = requests.get(uimage_url)
img = Image.open(BytesIO(response.content))
img.thumbnail((35,35), Image.ANTIALIAS)
buffered = BytesIO()
img.save(buffered, format="JPEG")
uimage_b64 = base64.b64encode(buffered.getvalue())

try:
    words = bio.split()
except:
    words = ['','-']
formatted_bio = ''


for index, val in enumerate(words):
    if index % 6 == 0 and index != 0:
        formatted_bio = formatted_bio + val +" |size=10 font='Ubuntu' \n"
    else:
        formatted_bio = formatted_bio + val + " "

print("IGStats|image='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAMFBMVEX///8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAv3aB7AAAAEHRSTlMAgHE1y/OdNmGHwxUFGSQs7GThJAAAAEdJREFUeJxjYMACjAQFlUE0Z5igYOoEIMN0AwPDvWAgQ42nxeNvEpAhyLG8qkEQxGApYHdAZcCkQIoPJEG1cwcjGwi3Ah0AAOCrEQEtfnOkAAAAAElFTkSuQmCC'")
print("---")
print(full_name+"|color=black size=20 href='%s' font='Marker Felt' image="+str(uimage_b64)) % ('https://instagram.com/'+user)
print("@"+user)
print(formatted_bio.encode('utf-8') + "| size=10 font='Ubuntu' ")
print("---")
print("Follower: %s Following: %s |color=black") % (follower_count,following_count)
print("---")
print("Posted media: " + str(media_count) +"|color=black")
print("%i last posts|size=20 color=black") % (last_post_count)
for last_media in last_medias:
    print("\n:green_heart: %s :speech_balloon: %s  | href='%s' image="+last_media['last_image_b64']) % (last_media['last_image_likes'],last_media['last_image_comments'],last_media['last_image_url'])
    print("--"+str(last_media['last_image_caption'].encode('utf-8')).rstrip('\r\n').replace('\n', ' ').replace('\r', ''))
print("---")
print("Refresh | refresh=true image='iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAADAFBMVEX///8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAmJiYnJycoKCgpKSkqKiorKyssLCwtLS0uLi4vLy8wMDAxMTEyMjIzMzM0NDQ1NTU2NjY3Nzc4ODg5OTk6Ojo7Ozs8PDw9PT0+Pj4/Pz9AQEBBQUFCQkJDQ0NERERFRUVGRkZHR0dISEhJSUlKSkpLS0tMTExNTU1OTk5PT09QUFBRUVFSUlJTU1NUVFRVVVVWVlZXV1dYWFhZWVlaWlpbW1tcXFxdXV1eXl5fX19gYGBhYWFiYmJjY2NkZGRlZWVmZmZnZ2doaGhpaWlqampra2tsbGxtbW1ubm5vb29wcHBxcXFycnJzc3N0dHR1dXV2dnZ3d3d4eHh5eXl6enp7e3t8fHx9fX1+fn5/f3+AgICBgYGCgoKDg4OEhISFhYWGhoaHh4eIiIiJiYmKioqLi4uMjIyNjY2Ojo6Pj4+QkJCRkZGSkpKTk5OUlJSVlZWWlpaXl5eYmJiZmZmampqbm5ucnJydnZ2enp6fn5+goKChoaGioqKjo6OkpKSlpaWmpqanp6eoqKipqamqqqqrq6usrKytra2urq6vr6+wsLCxsbGysrKzs7O0tLS1tbW2tra3t7e4uLi5ubm6urq7u7u8vLy9vb2+vr6/v7/AwMDBwcHCwsLDw8PExMTFxcXGxsbHx8fIyMjJycnKysrLy8vMzMzNzc3Ozs7Pz8/Q0NDR0dHS0tLT09PU1NTV1dXW1tbX19fY2NjZ2dna2trb29vc3Nzd3d3e3t7f39/g4ODh4eHi4uLj4+Pk5OTl5eXm5ubn5+fo6Ojp6enq6urr6+vs7Ozt7e3u7u7v7+/w8PDx8fHy8vLz8/P09PT19fX29vb39/f4+Pj5+fn6+vr7+/v8/Pz9/f3+/v7///87ptqzAAAAJXRSTlMAgA5ABAHjYRLswnooVM0CyLDK2mCpIMSvX5AFm5SRscBeH2Kql1edqgAAAGdJREFUeJyNjUcSgCAUQ1GKSlGw9879r6j4Wbogm0zeTBKEgkQSnrFmQBhDTstcyLbu+jH6cqENdT7NFoCq4s+t9QDFYU+/8t3oXYNcKQ8W4pwaXQBYt/04pcjLFCoY0+tmGU9I2NMDXoEEmA7BEvIAAAAASUVORK5CYII=' ")

