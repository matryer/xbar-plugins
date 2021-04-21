#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3

# <xbar.title>Today's Tweets</xbar.title>
# <xbar.version>v1.2.1</xbar.version>
# <xbar.author>Luke C.D. Stein</xbar.author>
# <xbar.author.github>lukestein</xbar.author.github>
# <xbar.desc>Display the tweets you've posted today.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/lukestein/bitbar_todays_tweets/master/todays_tweets.png</xbar.image>
# <xbar.dependencies>python3,tweepy</xbar.dependencies>
# <xbar.abouturl>https://github.com/lukestein/bitbar_todays_tweets</xbar.abouturl>

# See advice at
# https://github.com/matryer/xbar#writing-plugins
# http://docs.tweepy.org/en/v3.8.0/api.html
# https://www.iconfinder.com/
# https://onlinepngtools.com/convert-png-to-base64

import tweepy
import datetime
import time


# %% Twitter API keys
consumer_key        = "KEY"
consumer_secret     = "SECRET"
access_token        = "TOKEN"
access_token_secret = "TOKENSECRET"


# %% Parameters
showmentions = True
maxmentions  = 50


# %% Setup

def is_reply(t):
    return (t.in_reply_to_screen_name != None) and (t.in_reply_to_screen_name != screen_name)

twitterlogo = "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAArBJREFUWAntlT9rFUEUxV+MkQTFtCIJpE1npUhKJQoWImlThAREK7HwKwhWgn4KEW2sLNKmsBDBT6CWNnbmf3J+++5ZZ5/zdtaImGIvnJ07d+6fs3dmdicGg8GRcGrkzKlhEkR6QqUd6TvUd6jUgdL62ZJDrPOtmujgux8+nM3DiJkMnRzorUKR0ofRZFwgRwwfkLske7JPCYhzDWeZZy5B6uYEJMXXhXM+rL8R7gpLwprwUYDMd+GVkHsZmZviIrkRIthXhaehY7OdNbaJ8bEwKhyJFeGC8EjA70Cg2+g5ZI04Ooji8wLyTHASiPyM+XuNCAToCGfF2yS1kmt6fhaI90s4VzrWBVIjugltS58TLDelsBWp/8NYHCXBFtn2MmLayJCzkTgtgu6tWcdRMj0cqucNPZ8LvPWdyvL7LfItfqJ18u3GOFqnnjsg8jUGnLy+KJ1DS7ewcQ42Axrqw4o9FR/ihdRY0mt2ckx1b9mO7JcjiQswhRiA6Djxd4fbR253PK3T0NuSUZz9PifcExB3DJ01APGcEE/HIHUlHNrqhUuzKw228iAhth/CVQFJuzS05J/uznUtk4Nc7vponXpeYsw6bZ4VNgSkFDP0+uX3IAwQ6vQyNTsF5HRf03eu1CGxrzpbRU46U+xO+GZJpMRIxO3C9kJwh8a9bXrOPkWcXyrNO05vJZR7Kz6K8wKSkkLnAljeSqFo8dsTfhVBkqDkBDJ045uwGQ4fNPKb+BJz4p2DLiAXhdfCssD58/ZJ7SbjWschZO2rcKtDqhn53Bf4sxP3R52JmCO/neZZcZdYhBjd2RLoEOfqvLAgLAm3hUsCQrfSs1QZuzxKhMgBKdClAETYZnAi6ULIiSHlbwlxjsWO/ldEFF+Jk3r+38cTt/ZfMe8JlTrbd6jvUKkDpfVjvG0sZ2IOUeYAAAAASUVORK5CYII="

tweetlogo = "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAA8JJREFUWAntl7trVEEUxtdXfKJBxEchKAmijQixEAQTEbEUtdI0NopYaKFgk0ow/gEiKhamsLETrMQmpFBEkIho6QqCoOATxffj++2db5l73bm7N5vCIge+PWfmnsd3zszdTWq1GZmZQHcTmNVdeDKavHME5/8tG/wRKouTVA4MAXNLAuNn1InXjbBU8dl6SkdVxLmYwmJhh9AvkOepMCH8Ekzip2wLscnpLQxejLxTMRn8TwqvBQrEqGt9ULAQc0I4HDZy9ZxwkR5OCluD0/yg2ynHX5OjSfyQ/T2AaXj/iuwLwuew1yuN5Ah5jEN6QOBbYbOA9AgcYUoce0oOxH4VOBoTsIZUTOyD1hzpCqFPQNxYbV62rh2TdgI63Bf2Ufg0A8K+18u0ficQ24qMc6LJy726IdwW7gtLBcT5moSOaJOgL0FjXxKWCBaIMV6CPZ39sl0M3Q4cJT7vhZUCkjsyL3gzcGS0dOoRv5LNZY2Jadk4TvSIQBzdtyPjCfLWcWcRN5at9OlR8eC5YFJod4PN23NR2C0sFyzHZRR9WbeCSR8IwckXh6NA9gpOxFlj01VMjD0u/gNhTLgj2M+xKW1Cw4pB/pkOm36LBmSvEc4IJISQSbkgCX2UcVEfRbzXynbsHuVBfF0aCxPx5qB2Xwo7BS6cj1JmQ/CnI/wpBgkIop1LZlKIca0XSa/IaYvsuKt4OvH+VG1PkaZ9oYtNN3m6w/FAii+4qRZOxfn+3ApVPa2wzCs/3KRtd/JN9nROyS/G0VDaL1KeSbTyjd+mPb573Ol0kHKOT8q7KtT0qYRla2Wn1Xp8XngjQMwJTbKq9nSuKhfi5rNV4tMXjA4eC5PCR6FbQm6Gq9AnIG48W5V8mvk5+UCEznynqk7F/txF7FEBaXt3Mrfs01NidV1wUt4Qd+q9TrSP6gkJg8Q1vFeq44DT8uRnwsWrkPJrDqkNoaLf5rBMK0jEINCj5by3C3cFiHVyhJ4M/kMC4nzZqsPPeDoOobsxwR2XTYlnvjPoQQGpTIavcv8pwHTWC4eEm4KPC50iw9TiqTzSul9AKpHxRNYq8LJQF/hh9a8yJLDplqIGe0wMEuyZNH+8jwiWSmQcZFI92hgWJoS4iIuV6WeKOSv4W1hm85cduyMxEZy5uByHZZ2MXQKXeaNAIf4BJIZp8ebVhYfCuHBPYGoIU8GmgUoSE3IgX4wkYkKxsL9AIIZjglRRIEJc3FjRp3TdipADmBhAKFAsQizP0TTA88oTUUxOygjlHLUo+nZdvFjgv1z/BW5q39C+k8EQAAAAAElFTkSuQmCC"

retweetlogo = "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"

replylogo = "iVBORw0KGgoAAAANSUhEUgAAACYAAAAmCAYAAACoPemuAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAoRJREFUWAntl81qFEEUhaNRA5HgMmg2MeJGyNKFDyC4DfoWPkfIOmSRPIRugiCuhMGFIYqib5CQTXCtJlGT8zF15NI/M1WTOBOkL5yu21V17z11qrp7Zmqqs06BToH/R4ErWgq4VHY1sIl+6B6/O51KzqhdFW6Mn0K9okkx0hP205SJbmkk9UqEToWdSROLpF4nUhD7lIhNpGki9T2RMzHOGfPGtqVNpI5E4Fci9lFt1a6pI8ZVx7PuByVg7HfKwvY9EY4F1PE7bE7+A2Ep9R2qPRHYZuJ5leBfmEXCPlMoRRHjT/Ddt6e+TeGhYEPBC9niHFImAjlUBKjrfto3QiQY82qozJDe1qZULF71IVclue6EalGv2Cz3dUW+FSha3b4qkUH3nDWr+EX+HQEj/1CLCg2dXDjBZ4vFLQtfhfsChLPIad5fi0RH2co2Ba38N1W6naoVn7kYMIycDz8qAN5vbF/TE2tynzVu8/Hx/dA2h1xT8agYRKsPwk/1MccPROvDMIgx5Fg9hnLxBUtyYtmaF8KscEuYFxYEDnpcHEpiEGFBHJlHwnuBPo/LzbOYPG6rP0m7DWn4nXZPeCZsCXuClURFq9aTj8Vz3e/JvDaR80ec84JyPGXMa9oBPmErwjvBBH8k/7FarHVL+8Pt1yZyFPGvCxOiBahAsepr4an6DgQT3JaPxfz9noJrDC75oQhRSHrLOI8vBcih3KKAebx/V3iN5HqK3U/xFM+xqOCaAiD3PAXGsZxctTkmN+qfEeKtzqb8D6mC89YKlnQ4MTHRz82BwlZ5Q/7dFDhKrlrNmLw2mNFhEjc1dz7NN9mM8H87xeSocmlIecnnVd55urZToEiBMwks0+sAjBfRAAAAAElFTkSuQmCC"

mentionlogo = "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"

# %% Authenticate to Twitter:
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)


# %% Create API object
api = tweepy.API(auth)

while True:
    try:
        screen_name = api.me().screen_name
        pass
        break
    except:
        time.sleep(20)


#print("User @%s successfully authorized the app." % screen_name)


# %% Get timeline
tweets = []

for i in tweepy.Cursor(api.user_timeline).items():
    if ((i.created_at.replace(tzinfo=datetime.timezone.utc).astimezone(tz=None).month == datetime.datetime.now().month) and
        (i.created_at.replace(tzinfo=datetime.timezone.utc).astimezone(tz=None).day   == datetime.datetime.now().day)):
        tweets.append(i)
    else:
        break

tweetcount   = sum(1 for t in tweets if not t.retweeted and not is_reply(t))
retweetcount = sum(1 for t in tweets if     t.retweeted)
replycount   = sum(1 for t in tweets if not t.retweeted and     is_reply(t))


# %% Get mentions
if showmentions:
    mentions = []
    mentioncount = 0

    for i in tweepy.Cursor(api.mentions_timeline).items():
        if ((i.created_at.replace(tzinfo=datetime.timezone.utc).astimezone(tz=None).month == datetime.datetime.now().month) and
            (i.created_at.replace(tzinfo=datetime.timezone.utc).astimezone(tz=None).day   == datetime.datetime.now().day)   and
            mentioncount < maxmentions):

            mentioncount += 1
            mentions.append(i)
        else:
            break



### %% Get quote-tweets
#qtquery = "twitter.com%%2F%s%%2Fstatus%%20-(from%%3A%s)" % (screen_name, screen_name)
#print(qtquery)
#qts = api.search(qtquery, results_type="recent")
#for t in qts:
#    print(t.text)


# %% Format menu items, including colors if above daily tweet quota
prefix = ""
total_color   = "color=red" if (tweetcount + retweetcount >= 10) else ""
tweet_color   = "color=red" if (tweetcount >= 10)                else ""
retweet_color = "color=red" if (retweetcount >= 5)               else ""
reply_color   = "color=red" if (replycount >= 10)                else ""

# %% Print counts
print(prefix + "%d+%d+%d (💕%d) | templateImage=%s %s" % (tweetcount, retweetcount, replycount, sum(t.favorite_count for t in tweets), twitterlogo, total_color))

# %% Print tweets
print("---")

print(prefix + "%d Tweets (💕%d) | href=https://twitter.com/%s image=%s %s" % (tweetcount, sum(t.favorite_count for t in tweets if not t.retweeted and not is_reply(t)), screen_name, tweetlogo, tweet_color))
for t in tweets:
    if not t.retweeted and not is_reply(t):
        print("--%d💕 %s | size=12 length=60 href=https://twitter.com/%s/status/%d" % (t.favorite_count, t.text.replace('\n', ' ').replace('\r', ''), t.user.screen_name, t.id))

print(prefix + "%d Retweets | href=https://twitter.com/%s image=%s %s" % (retweetcount, screen_name, retweetlogo, retweet_color))
for t in tweets:
    if t.retweeted:
        print("--%s | size=12 length=60 href=https://twitter.com/%s/status/%d" % (t.text[3:].replace('\n', ' ').replace('\r', ''), t.user.screen_name, t.id))

print(prefix + "%d Replies (💕%d) | href=https://twitter.com/%s/with_replies image=%s %s"  % (replycount, sum(t.favorite_count for t in tweets if not t.retweeted and is_reply(t)), screen_name, replylogo, reply_color))
for t in tweets:
    if not t.retweeted and is_reply(t):
        print("--%d💕 %s | size=12 length=60 href=https://twitter.com/%s/status/%d" % (t.favorite_count, t.text.replace('\n', ' ').replace('\r', ''), t.user.screen_name, t.id))

mentionprefix = "≥" if (mentioncount == maxmentions) else ""

if showmentions:
    print("---")
    print(prefix + mentionprefix + "%d Mentions | href=https://twitter.com/notifications/mentions image=%s" % (mentioncount, mentionlogo))
    for t in mentions:
        print("--%d💕 %s: %s | size=12 length=60 href=https://twitter.com/%s/status/%d" % (t.favorite_count, t.user.screen_name, t.text.replace('\n', ' ').replace('\r', ''), t.user.screen_name, t.id))


print("---")
print("Analytics | href=https://analytics.twitter.com/user/%s/tweets" % screen_name)
print("%d Followers| href=https://twitter.com/%s/followers" % (api.me().followers_count, screen_name))
print("%d Following | href=https://twitter.com/%s/following" % (api.me().friends_count, screen_name))
