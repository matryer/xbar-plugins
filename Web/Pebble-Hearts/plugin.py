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

#define required variables
#only uncomment if either or both appids are defined
#appid1 = '5717ea24a86f0f5c67000008'
appid2 = '539e18f21a19dec6ca0000aa'

#Get Heartcount of 1st App
def getcount1():
    try:
        #look if appid1 was defined
        appid1
    except NameError:
        #if it wasnt...
        response1 = urllib2.urlopen('http://pblweb.com/api/v1/hearts/' + appid1 + '.json')
        mydata1 = response1.read()
        return mydata1
    else:
        #if it was...
        response1 = urllib2.urlopen('http://pblweb.com/api/v1/hearts/' + appid1 + '.json')
        mydata1 = response1.read()
        return mydata1

def getcount2():
    try:
        #look if appid2 was defined
        appid2
    except NameError:
        #if it wasnt...
        response2 = urllib2.urlopen('http://pblweb.com/api/v1/hearts/' + appid2 + '.json')
        mydata2 = response2.read()
        return mydata2
    else:
        #if it was...
        response2 = urllib2.urlopen('http://pblweb.com/api/v1/hearts/' + appid2 + '.json')
        mydata2 = response2.read()
        return mydata2

try:
    #look if the function for getting the heart amount for the 1st app returned something
    getcount1()
except NameError:
    #if it didnt...
    appid1check = 0
else:
    #if it did...
    appid1check = 1
    hearts1 = getcount1()
    w1 = json.loads(hearts1)
    heartcount1 = (w1['hearts'])

try:
    #look if the function for getting the heart amount for the 2nd app returned something
    getcount2()
except NameError:
    #if it didnt...
    appid2check = 0
else:
    #if it did...
    appid2check = 1
    hearts2 = getcount2()
    w2 = json.loads(hearts2)
    heartcount2 = (w2['hearts'])


#if both functions returned heart amounts
if appid1check == 1 and appid2check == 1:
    print "<3|color=#f23400 dropdown=false"
    print "---"
    print "App 1 has", heartcount1, "hearts."
    print "App 2 has", heartcount2, "hearts."

#if only the first function returned heart amounts
if appid1check == 1 and appid2check == 0:
    print heartcount1, "|color=#f23400 dropdown=false"
    print "---"

#if only the second function returned heart amounts
if appid1check == 0 and appid2check == 1:
    print heartcount2, "|color=#f23400 dropdown=false"
    print "---"

#if none of the functions returned heart amounts
if appid1check == 0 and appid2check == 0:
    print "N/A|color=#f23400 dropdown=false"
    print "---"
