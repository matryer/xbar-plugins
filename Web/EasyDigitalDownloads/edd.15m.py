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

domain = 'https://domain.co'
api_key = 'yourAPIkey'
api_token = 'yourAPItoken'

def get_edd():

  if api_key == "":
    return False

  edd_earnings = json.load( urllib2.urlopen( domain + '/edd-api/stats/?key=' + api_key + '&token=' + api_token + '&type=earnings' ) )
  edd_sales = json.load( urllib2.urlopen( domain + '/edd-api/stats/?key=' + api_key + '&token=' + api_token + '&type=sales' ) )

  try:
    response = {
        'earnings': edd_earnings['earnings'],
        'sales': edd_sales['sales']
    }
  except KeyError:
    return False

  return response

try:
  edd_data = get_edd()

  print 'Today: ${0:,.2f} from {1:,.0f} sales'.format( edd_data['earnings']['today'], float(edd_data['sales']['today']) )
  print '---' # Show each of the next lines in a drop-down
  print 'Current Month: ${0:,.2f} from {1:,.0f} sales'.format( edd_data['earnings']['current_month'], float(edd_data['sales']['current_month']) )
  print 'Last Month: ${0:,.2f} from {1:,.0f} sales'.format( edd_data['earnings']['last_month'], float(edd_data['sales']['last_month']) )
  print 'Total: ${0:,.2f} from {1:,.0f} sales'.format( edd_data['earnings']['totals'], float(edd_data['sales']['totals']) )

except Exception as inst:
  print 'Error fetching stats'
  print '---'
  print type(inst)     # the exception instance
  print inst.args      # arguments stored in .args
  print inst
