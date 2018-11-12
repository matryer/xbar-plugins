#!/usr/bin/env python

# <bitbar.title>F1 Countdown</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>DE</bitbar.author>
# <bitbar.author.github>destroy-everything</bitbar.author.github>
# <bitbar.desc>Shows countdown to next F1 event normalised to UTC.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/3232EJx.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

#SETUP INSTRUCTIONS
# 1:
#To set up your environment for this you need to download 
#https://github.com/collective/icalendar/archive/3.9.2.zip
#extract the above then go into the directory and run the following:
#sudo python setup.py install
#all other dependencies are core
# 2:
#Now we just need to download the calendar file and tell the script where you put it
#http://www.f1calendar.com/download/f1-calendar_p1_p2_p3_q_gp.ics?t=1461060079
#Download this file and save it somwhere the script can get to it and the update the below 'calfile' variable with the full path to the file
#Dont put the cal file in the plugins folder, you can create a sub-directory like I have else you will break all your plugins

calfile = "/Users/me/Documents/BitbarPlugins/f1data/f1-calendar_p1_p2_p3_q_gp.ics"

###################################################
###################################################
#### you dont need to change anything past here ###
###################################################
###################################################

from icalendar import Calendar
#from datetime import datetime
from datetime import datetime
from pytz import timezone
import pytz
import re
import tzlocal  # $ pip install tzlocal

#import urllib2
#import requests
#GOD DAM OSX ships with old version of py cant get to HTTPS services without breaking

#DATE = '2016-04-29 18:30:00'
now = datetime.utcnow()
#calfile = requests.get('http://www.f1calendar.com/download/f1-calendar_p1_p2_p3_q_gp.ics?t=1461060079')
#calfile = urllib2.urlopen("http://www.f1calendar.com/download/f1-calendar_p1_p2_p3_q_gp.ics?t=1461060079")
#with open('/Users/C5066492/Documents/BitBarPlugins/cal/f1-calendar2016.ics','wb') as output:
#output.write(calfile.read())

utc=pytz.UTC


g = open(calfile,'rb')
gcal = Calendar.from_ical(g.read())
for component in gcal.walk():
	if component.name == "VEVENT":

			if component.get('dtstart').dt > utc.localize(now) :
				DATE = component.get('dtstart').dt
				summary = component.get('summary')
				location = component.get('location')
				matchObjFP1 = re.search( r'First.*Practice.*', summary, re.M|re.I)
				matchObjFP2 = re.search( r'Second.*Practice.*', summary, re.M|re.I)
				matchObjFP3 = re.search( r'Third.*Practice.*', summary, re.M|re.I)
				matchObjQ = re.search( r'Qualifying.*', summary, re.M|re.I)
				matchObjGP = re.search( r'Grand.*Prix.*', summary, re.M|re.I)
				
				if matchObjFP1:
						#print "FIRST PRACTICE1 : ", matchObj.group()
						event = "FP1:"

				elif matchObjFP2:
						event = "FP2:"
				elif matchObjFP3:
						event = "FP3:"
				elif matchObjQ:
						event = "QUALI:"
				elif matchObjGP:
						event = "GP:"
				break

g.close()


def dateDiffInSeconds(date1, date2):
    timedelta = date2 - date1
    return timedelta.days * 24 * 3600 + timedelta.seconds


def daysHoursMinutesSecondsFromSeconds(seconds):
    minutes, seconds = divmod(seconds, 60)
    hours, minutes = divmod(minutes, 60)
    days, hours = divmod(hours, 24)
    #return (days, hours, minutes)
    return (days, hours, minutes, seconds)
now = datetime.now()
us_tz = timezone(tzlocal.get_localzone().zone)
localized_end_date = us_tz.normalize(DATE.astimezone(us_tz))
print event,"%dd %dh %dm %ds" % daysHoursMinutesSecondsFromSeconds(dateDiffInSeconds(utc.localize(now), localized_end_date  )), "|size=12 color=#95949e"
#  %ds
print "---"
print "---"
print summary 
print location 
