#!/usr/bin/env python

# <bitbar.title>Pebble Hearts</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Bastian</bitbar.author>
# <bitbar.author.github>phntxx</bitbar.author.github>
# <bitbar.desc>Shows the amount of hearts your pebble app/watchface has.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/PHNTXX/bitbar_pebblehearts/</bitbar.abouturl>

#import required modules
import urllib2
import json

# Add your AppIDs here!
appIds = []


def getData(appId):
    response = urllib2.urlopen('https://appstore-api.getpebble.com/v2/apps/id/' + appId)
    data = response.read()
    try:
        appData = json.loads(data)['data'][0]
        return {'id': appId,
                'title': appData['title'],
                'hearts': appData['hearts'],
                'link': appData['links']['share']}
    except:
        return None

hearts = []
for app in appIds:
    hearts.append(getData(app))

print ":heart: %d | emoji=true" % sum([x['hearts'] for x in hearts])
print "---"
for h in hearts:
    print "%s has %s :heart: | href='%s' emoji=true" % (h['title'], h['hearts'], h['link'])
