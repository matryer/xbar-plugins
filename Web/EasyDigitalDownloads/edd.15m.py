#!/usr/bin/python
# -*- coding: utf-8 -*-

# Easy Digital Downloads
#
# <bitbar.title>Easy Digital Downloads</bitbar.title>
# <bitbar.author>Zack Katz (zack@katz.co)</bitbar.author>
# <bitbar.author.github>zackkatz</bitbar.author.github>
# <bitbar.image></bitbar.image>
#
# Fetch EDD sales for the day. Change your API domain, key, token, and currency as necessary.
#
# See http://docs.easydigitaldownloads.com/article/1135-edd-rest-api---stats for the API used
# See http://docs.easydigitaldownloads.com/article/1131-edd-rest-api-introduction for general REST API info

import json
import urllib2
import calendar
import datetime
from datetime import timedelta

domain = 'https://domain.co'
api_key = 'yourAPIkey'
api_token = 'yourAPItoken'

def get_edd():

  if api_key == "":
    return False

  opener = urllib2.build_opener()
  opener.addheaders = [('User-Agent', 'Mozilla/5.0')]
  edd_stats = json.load( opener.open( domain + '/edd-api/stats/?key=' + api_key + '&token=' + api_token ) )

  try:
    response = edd_stats
  except KeyError:
    return False

  return response

try:
  edd_data = get_edd()

  # Get how many days have passed this month
  now = datetime.datetime.now()
  days_so_far = now.day

  # Get how many days there were last month
  last_month = now.replace(day=1) - timedelta(days=1)
  days_last_month = calendar.monthrange(last_month.year, last_month.month)[1]

  print 'Today: ${0:,.2f} from {1:,.0f} sales'.format( edd_data['stats']['earnings']['today'], float(edd_data['stats']['sales']['today']) )
  print '---' # Show each of the next lines in a drop-down
  print 'Current Month: ${0:,.2f} from {1:,.0f} sales'.format( edd_data['stats']['earnings']['current_month'], float(edd_data['stats']['sales']['current_month']))
  print '${0:,.2f} daily average, ${1:,.2f} ASP'.format( float( edd_data['stats']['earnings']['current_month'] / days_so_far ), float(edd_data['stats']['earnings']['current_month'] / int(edd_data['stats']['sales']['current_month'])))
  print 'Last Month: ${0:,.2f} from {1:,.0f} sales'.format( edd_data['stats']['earnings']['last_month'], float(edd_data['stats']['sales']['last_month']) )
  print '${0:,.2f} daily average, ${1:,.2f} ASP'.format( float(edd_data['stats']['earnings']['last_month'] / days_last_month), float(edd_data['stats']['earnings']['last_month'] / int(edd_data['stats']['sales']['last_month'])))
  print 'Total: ${0:,.2f} from {1:,.0f} sales'.format(edd_data['stats']['earnings']['totals'], float(edd_data['stats']['sales']['totals']))

except Exception as inst:
  print 'Error fetching stats'
  print '---'
  print type(inst)     # the exception instance
  print inst.args      # arguments stored in .args
  print inst
