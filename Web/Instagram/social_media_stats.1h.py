#!/usr/bin/env LC_ALL=en_US.UTF-8 /usr/local/bin/python3
#
# Icons made by Freepik (https://www.flaticon.com/authors/freepik)
#
# <bitbar.title>Social Media Stats</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Long Do</bitbar.author>
# <bitbar.author.github>longpdo</bitbar.author.github>
# <bitbar.desc>Shows YouTube subscribers, Facebook likes, Instagram and Twitter followers.</bitbar.desc>
# <bitbar.image>https://github.com/longpdo/bitbar-plugins-custom/raw/master/images/social_media_stats.png</bitbar.image>
# <bitbar.dependencies>python3,beautifulsoup4,requests</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/longpdo/bitbar-plugins-custom</bitbar.abouturl>
#
# by longpdo (https://github.com/longpdo)

import json
import requests
from bs4 import BeautifulSoup
import re

# Social URLs
# Leave the url of the social media you don't want to track empty
# e.g. youtube_url = ''
instagram_url = 'https://www.instagram.com/belle.dara'
facebook_url = 'https://facebook.com/Cristiano'
twitter_url = 'https://twitter.com/billieeilish'
youtube_url = 'https://www.youtube.com/channel/UCtxCXg-UvSnTKPOzLH4wJaQ'


# Base64 Colored Icons
instagram_icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAWQAAAFkBqp2phgAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAJESURBVDiNjZGxaxRREMZ/s7cPRbJv30W0CwELFVIEzkhSSESw0ZAiXSxTmUiK4B8gdhYWBhExVUoFkSAhXZqkSkACpzmwECXYWWTf7l2j73bHIpeTiIoDXzEw34+Zb0RVQUQCQ/NB7ExFMlJibZfEqFpTYimxoaeii22VpGuXuPUCRUVBENZL7FRFutmVpFlW1pekoWQgVCR0SU2JNRWJq0hHu9ibJXbjIrenRS+wQMZzMu6g+op/lbyuHZCdV+R6SfKyS3ovImUGx+YfzTJmkDFz3Hr2H5+h+CroVkyxWSOfiXCM4GieNLqGRsPbyveOcrpTytQ2MtuoUbwxtJ8OU/8WkTdjipEYh0XwffM5aSDsCHSqSJ5RKRHMVbCTwAT65D5AJA88iI1xGCD0ASnLVHQ4zIejyrePNrr8EDgoYbkGkwA1iiBgIhyGtAcYE0OdcRyrqLb7UP3YVvxqRD5+nElMHmoUJsYB0hu8Anz5yweiDlQCnDpqaQNChCOQcpT0igYcu9SZ45okv0KVJFI/hxS76LtwdELHiBQhxhEQ+q+izhLCDnDAVVnlEBhkjiwfEGXpeKxGbkBDhKPA4vqAFd3DMYFjnzqLDLJInX3OMoH6vT4gKlyNoohxtIDRE/eu6B4wyV0xHAKfNPBbxVo0FGmhTRZ0C9W3zKoq/6WIR12x+oOhBVFF+MA6GVN4NsloUuDJCOQEDoEcQ44hw5HTIOMGng2UaVFVQIT3zOOZwTOCx+IxPYEn9FTgaZGzxmdeoKo/ARK4FtA9gmsgAAAAAElFTkSuQmCC'
facebook_icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAWQAAAFkBqp2phgAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAEISURBVDiNY3RK33yCgeG/OQNZgPEkE/maGRgYGP6bsxCrVE6Ch8HJVJqBk52F4fjllwwXbr5hYGBgYCDKgKYMUwZrAwk4/9uPP8QboKcqDNf84fMvhuv33zM8fPEZLk/QAHlJHji7oOcow+OXX1DkmQgZwMbKDGe///wTQx6nC+yNpRgsdMQY5CV54WL5kboMf/78Y9hx/DHDxVtv8RugJsfP4GYpiyLmZCrNwMDAwHDw3HPCLnj78QfD3SefGAR42RiE+TkYGBgYGO4/+8zw799/hscvEOHA6JS+6T8uQxgYGBiCnZUYskK1GRgYGBj8i3YwfPn2G0WeYCASAsPDAMaT5GtnPAkAC5ZLEBbqG4cAAAAASUVORK5CYII='
twitter_icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAWQAAAFkBqp2phgAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAFMSURBVDiNpdM/axRBGAbw360nCGEOhDSiWVEsgjYWVoIiSIroB0g3ECxSp7EIVrFQRL9CIIuddoJNBAuLQAghEE0TxDCghaULIaBhLW6C8dgcF31gmOGZ93n/znSapvE/6I5iVFb1eUyjwEqK4Uvme52J5R9jeIyHKYZfLeIpvEYvU/tYxmXMFLiNeayUVT0+ID6VjXtH6DOYQwf3C3zNF3ewXlb1bFnVFzJ3DeeOqWw3xfCyiy18wC1cxFKOvo1PQ1rzin4Tr6NtFFfzOg6b9Lu6mSOfBN9SDN+hSDE0eIDPJ3Dw/vBQ5H0V7/BzBPE+Hv3lIMWwhx2cHsHBsxTD7mAGUgwvcANvhohX8fQo0YWyqs/qz3wW91qEDZ5jYfC1dsuqvolF3G0R7mEDT1IMb9tS6hz+xrKqL2ESV/QbtYaPKYaDISX9cfCv+A2xmlzhJgjeHgAAAABJRU5ErkJggg=='
youtube_icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAWQAAAFkBqp2phgAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAADHSURBVDiNxdMtTgNREMDx33tBApKtwqwlmCYV1SRwj56hx+kVeosuSSXBrqnqWkBAzWC6CeKVLF3BJP+MmK/MV4oIYySPisYFSOkWd7guAG8FXkXsBI/BVxB/5DN4EKzPCO5ZZ0yKzdX1kBFMMqqiablks2E6/S1BdToBzOdst6xWVEW3m7FrzBndSXPTMJuxWNAV3TpBU5xwXQ/ZwnPGvli9bYe0sO8P6XDGDRyCpxQRpFTjHlc/uDxqeMfHUfe8iGjTv3/jNy6Ep9IgbWhqAAAAAElFTkSuQmCC'


def get_instagram_followers():
    if instagram_url == '':
        return

    request = requests.get(instagram_url)
    if request.status_code != 200:
        return

    if request.status_code == 200:
        soup = BeautifulSoup(request.text, 'html.parser')
        data = json.loads(soup.find('script', type='application/ld+json').text)
        result = data['mainEntityofPage']['interactionStatistic']['userInteractionCount']
        result = '{0:,}'.format(int(result))
        result = result.replace(',', '.')
        print(result + ' | image=' + instagram_icon)


def get_facebook_likes():
    if facebook_url == '':
        return

    request = requests.get(facebook_url)
    if request.status_code != 200:
        return

    if request.status_code == 200:
        soup = BeautifulSoup(request.text, 'html.parser')
        data = soup.find('span', {'id': 'PagesLikesCountDOMID'})
        result = re.sub('[^0-9.]+', '', str(data.text))
        print(result + ' | image=' + facebook_icon)


def get_twitter_followers():
    if twitter_url == '':
        return

    request = requests.get(twitter_url)
    if request.status_code != 200:
        return

    if request.status_code == 200:
        soup = BeautifulSoup(request.text, 'html.parser')
        data = soup.find('li', {'class': 'ProfileNav-item--followers'})
        result = re.sub('[^0-9.]+', '', str(data.find('a')['title']))
        print(result + ' | image=' + twitter_icon)


def get_youtube_subscribers():
    if youtube_url == '':
        return

    request = requests.get(youtube_url)
    if request.status_code != 200:
        return

    if request.status_code == 200:
        soup = BeautifulSoup(request.text, 'html.parser')
        data = soup.find('span', {'class': 'subscribed'})
        result = re.sub('[^0-9.,MK]+', '', str(data['aria-label']))
        if result[-1] == '.':
            result = result[:-1]
        print(result + ' | image=' + youtube_icon)


if __name__ == '__main__':
    get_instagram_followers()
    get_facebook_likes()
    get_twitter_followers()
    get_youtube_subscribers()
