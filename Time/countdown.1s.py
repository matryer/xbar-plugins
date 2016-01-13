#!/usr/bin/env python

# <bitbar.title>countdown</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Pere Albujer</bitbar.author>
# <bitbar.desc>Shows countdown of established date.</bitbar.desc>

from datetime import datetime, time

# Set date in format using this format: yyyy-mm-dd hh:mm:ss
DATE = '2017-12-31 00:00:00'


def dateDiffInSeconds(date1, date2):
    timedelta = date2 - date1
    return timedelta.days * 24 * 3600 + timedelta.seconds


def daysHoursMinutesSecondsFromSeconds(seconds):
    minutes, seconds = divmod(seconds, 60)
    hours, minutes = divmod(minutes, 60)
    days, hours = divmod(hours, 24)
    return (days, hours, minutes, seconds)

leaving_date = datetime.strptime(DATE, '%Y-%m-%d %H:%M:%S')
now = datetime.now()

print "Countdown: %d d, %d h, %d m, %d s" % daysHoursMinutesSecondsFromSeconds(dateDiffInSeconds(now, leaving_date))
