#!/usr/bin/python
# -*- coding: utf-8 -*-
# <xbar.title>Home Assistant</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>muuuh</xbar.author>
# <xbar.desc>...</xbar.desc>
# <xbar.image>...</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

####################
# User Configuration

#http://192.168.1.100:8123/profile
homeassistant_api_key = "..."

if homeassistant_api_key == "":
	raise SystemExit("API Key Required")

def toggleplug():
	from requests import post
	import json
	url = "http://192.168.1.100:8123/api/services/switch/toggle"
	headers = {
		'Authorization': 'Bearer ' + homeassistant_api_key,
		'content-type': 'application/json',
	}
	payload = {'entity_id': 'switch.plug_switch'}
	result = post(url, headers=headers, data=json.dumps(payload))
	print(result.text)

import sys, getopt
opts, args = getopt.getopt(sys.argv[1:], 't:', ['toggleplug'])
for o, a in opts:
	if o == '--toggleplug': toggleplug(); exit();

#Default

from requests import get
import json
from collections import OrderedDict

try:
	url = "http://192.168.1.100:8123/api/states"
	headers = {
		'Authorization': 'Bearer ' + homeassistant_api_key,
		'content-type': 'application/json',
	}
	result = get(url, headers=headers)
	#print result.text.decode("utf-8")
	json = json.loads(result.text.decode("utf-8"), object_pairs_hook=OrderedDict)
except Exception, err:
	print("---")
	raise SystemExit(err)

for item in json:
	if item['entity_id'] == 'sensor.sensor1_temperature':
		sensor1_temperature = item
	if item['entity_id'] == 'sensor.sensor1_humidity':
		sensor1_humidity = item
	if item['entity_id'] == 'switch.plug_switch':
		plug_switch = item

from datetime import datetime

sensor1_temperature_value = (sensor1_temperature_temperature['state'] + " " + sensor1_temperature_temperature['attributes']['unit_of_measurement']).encode('utf8')
sensor1_temperature_changed_minutes = str((datetime.utcnow() - datetime.strptime(sensor1_temperature_temperature['last_changed'], '%Y-%m-%dT%H:%M:%S.%f+00:00')).seconds / 60).encode('utf8')

sensor1_humidity_value = (sensor1_humidity_temperature['state'] + " " + sensor1_humidity_temperature['attributes']['unit_of_measurement']).encode('utf8')
sensor1_humidity_changed_minutes = str((datetime.utcnow() - datetime.strptime(sensor1_humidity_temperature['last_changed'], '%Y-%m-%dT%H:%M:%S.%f+00:00')).seconds / 60).encode('utf8')

plug_switch_value = (plug_switch['state']).encode('utf8')

# Print the data
print(sensor1_temperature_value + "| font='SF Compact Text Regular'")

print("---")
print("Sensor1: " + sensor1_temperature_value + " (" + sensor1_temperature_changed_minutes + " min) | href:http://192.168.1.100:8123/")

print("---")
print("Sensor1: " + sensor1_humidity_value + " | href:http://192.168.1.100:8123/")

print("---")
print "Plug: " + plug_switch_value + ' | terminal=false bash="%s" param1=--toggleplug refresh=true' % sys.argv[0]
