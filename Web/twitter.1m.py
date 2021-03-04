#!/usr/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>View Tweets</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>kylebx</bitbar.author>
# <bitbar.author.github>kylebx</bitbar.author.github>
# <bitbar.desc>View the latest 5 tweets from your Twitter timeline.</bitbar.desc>
# <bitbar.image></bitbar.image>
# <bitbar.dependencies>python, tweepy, json</bitbar.dependencies>

import tweepy, json

# establish api connection with keys
auth = tweepy.OAuthHandler('consumer_key', 'consumer_secret')
auth.set_access_token('access_token', 'access_token_secret')
api = tweepy.API(auth)

def getTweet():
	try:
		print "Twitter"
		print "---" 
		tweet_list = api.home_timeline(count=5) # get the latest 5 tweets from the auntenicating user
		for i in range(0,len(tweet_list)): # for 5 most recent tweets
			status = tweet_list[i] # set the status equal to the first tweet in the array of tweets
			json_str = json.dumps(status._json) # the json string is the dump of the status converted to a json like format
			jsonTweet = json.loads(json_str) # json tweet becomes an actual json object using json.loads()
			tweet = jsonTweet['text'] # tweet is the text of the said tweet
			userInfo = jsonTweet['user'] # twitter handle is held within userUnfo so set that
			tweeter = userInfo['screen_name'] # retrieve the twitter handle
			print "@{u}: {tweet}".format(u=tweeter, tweet = tweet.encode('utf-8')) # print the username and the tweet
			print "---"
	except Exception, e:
		print "❌Twitter"
		print "---"
		try:
			print "[!] Message from Twitter: {msg}".format(msg = e.message[0]['message'])
		except Exception, e:
			print "Unknown Error."

def main():
	getTweet()

if __name__ == "__main__":
	main()
