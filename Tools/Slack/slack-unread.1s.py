#!/usr/bin/env python
# coding=utf-8
#
# <bitbar.title>Slack Notification</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>mgjo5899</bitbar.author>
# <bitbar.author.github>mgjo5899</bitbar.author.github>
# <bitbar.desc>Displays number of unread Slack messages</bitbar.desc>
# <bitbar.image>https://i.imgur.com/I3MdNmU.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# by mgjo5899

try:
	import requests
except ImportError:
	print('You need to `pip install requests`')

import json
from time import sleep
from threading import Thread

#
# USER TOKEN
# You need to modify this part by generating your own tokens
# https://api.slack.com/custom-integrations/legacy-tokens#legacy_token_generator
#
tokens = [
	# Replace this line
	'xoxp-slack-token-place-holder',
]

if tokens == ['xoxp-slack-token-place-holder']:
	import sys

	sys.exit('You need to generate a Slack user token and specify it in the "slack-unread.1s.py" script. \nVisit api.slack.com | color=#09f href=https://api.slack.com/custom-integrations/legacy-tokens#legacy_token_generator')

dark_mode = False
# Build script in terminal
debug_mode = False

blank_icon = ' image=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'
color_icon = ' image=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'
slack_call_command = ' href=slack:'

channels_list_url = 'https://slack.com/api/channels.list'
channels_info_url = 'https://slack.com/api/channels.info'

groups_list_url = 'https://slack.com/api/groups.list'
groups_info_url = 'https://slack.com/api/groups.info'

conversations_list_url = 'https://slack.com/api/conversations.list'
conversations_info_url = 'https://slack.com/api/im.history'

unread_count = 0


# When there are many channels under one Slack group, due to request rate limit per group,
# some come out with "ratelimit exceeded error" returned.
# So I had to run channel info request as fast as possible for each group
# ONE TOKEN REPRESENT ONE SLACK GROUP
def get_unread_count(url, token, channel_id):
	r = requests.get(url + '?token=' + token + '&channel=' + channel_id + '&unreads=true')
	return r

def get_list(url, scope, scope_type = None):
	if debug_mode:
		print('get_list: ' + url)

	try:
		r = requests.get(url)

	except Exception as e:
		if debug_mode:
			print('get_list error')
			print(e)

		return

	result = json.loads(r.text)[scope]
	ids = []

	for channel in result:
		# Filter
		if (
			# Ignore slack user
			'user' in channel and channel['user'] == 'USLACKBOT' or
			# Ignore deleted users
			'is_user_deleted' in channel and channel['is_user_deleted'] == True or
			# Ignore channels that don't have me
			'is_member' in channel and channel['is_member'] != True
		):
			continue

		# Ignore channels for private messages
		if scope_type == 'user' and not 'user' in channel:
			continue

		ids.append(channel['id'])

	return ids

def get_channel_unreads(url, token, id, scope = None):
	global unread_count

	r = get_unread_count(url, token, id)

	try:
		if scope != None:
			channel = json.loads(r.text)[scope]
		else:
			channel = json.loads(r.text)

		# Only for channels
		if 'is_member' in channel and channel['is_member'] == False:
			return

		if 'unread_count_display' in channel.keys():
			unread_count += channel['unread_count_display']

		elif 'unread_count' in channel.keys():
			unread_count += channel['unread_count']

	except Exception as e:
		if debug_mode:
			print('Error: ')
			print(e)

		sleep(2)
		
		
def groups(token):
	all_threads = []
	for id in get_list(groups_list_url + '?token=' + token + '&exclude_archived=true', 'groups'):
		if debug_mode:
			print('Loading groups unreads: %s, id: %s' % (token, id))

		t = Thread(target=get_channel_unreads, args=(groups_info_url, token, id, 'group'))
		t.start()
		all_threads.append(t)

	for t in all_threads:
		t.join()


def channels(token):
	all_threads = []
	for id in get_list(channels_list_url + '?token=' + token + '&exclude_archived=true', 'channels'):
		if debug_mode:
			print('Loading channel unreads: %s, id: %s' % (token, id))

		t = Thread(target=get_channel_unreads, args=(channels_info_url, token, id, 'channel'))
		t.start()
		all_threads.append(t)

	for t in all_threads:
		t.join()


def privates(token):
	all_threads = []
	for id in get_list(conversations_list_url + '?token=' + token + '&types=public_channel,private_channel,mpim,im', 'channels', 'user'):
		if debug_mode:
			print('Loading private channel unreads: %s, id: %s' % (token, id))

		t = Thread(target=get_channel_unreads, args=(conversations_info_url, token, id))
		t.start()
		all_threads.append(t)

	for t in all_threads:
		t.join()
		
		
for token in tokens:
	if debug_mode:
		print('Processing token: %s' % (token))
	g = Thread(target=groups, args=(token,))
	c = Thread(target=channels, args=(token,))
	p = Thread(target=privates, args=(token,))
	g.start()
	c.start()
	p.start()

	g.join()
	c.join()
	p.join()

# THIS IS A SPECIAL FEATURE FOR USERS WITH DARK MENU BAR ENABLED
# REMOVE COMMENTING OUT TO USE
if unread_count == 0:
	if dark_mode:

		if debug_mode:
			print('0 - blank_icon')
		else:
			# Empty white icon
			print(" | " + blank_icon + slack_call_command)

	else:
		if debug_mode:
			print('0 - color_icon')
		else:
			print(" | " + color_icon + slack_call_command)

else:
	if debug_mode:
		print(str(unread_count) + ' - color icon')
	else:
		print(str(unread_count) + " | " + color_icon + slack_call_command)
