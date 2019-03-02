#!/usr/bin/python
# coding=utf-8
#
# <bitbar.title>Twitter stats</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>impshum</bitbar.author>
# <bitbar.author.github>impshum</bitbar.author.github>
# <bitbar.desc>Displays Twitter followers & friends stats</bitbar.desc>
# <bitbar.image>https://i.imgur.com/Y2ljulD.png/bitbar.image>
#
# by impshum

import tweepy


consumer_key = 'yfbSqfbyJENjYPd8uEKJRjtQK'
consumer_secret = 'cmI9xT8Suqq2DdEZkKpjipy3d6Gw4AxyQlccI8uOM1MyV7m8OD'
access_key = '3809252535-Sjdoi7Sri0ruF6KFpQl4XlInGXlan1WHOK2IYyV'
access_secret = 'R8udOhIwRMgESoLcDdVklUANXMw9PQUBIz1jVMR7T7hIK'


auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)
me = api.me()

friends = me.friends_count
followers = me.followers_count

print('TW-{}/{}'.format(followers, friends))
