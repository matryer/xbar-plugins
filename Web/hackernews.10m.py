#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# <bitbar.title>Hacker News</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Liam Scalzulli</bitbar.author>
# <bitbar.author.github>terror</bitbar.author.github>
# <bitbar.desc>View top Hacker News articles in your status bar!</bitbar.desc>
# <bitbar.image>https://reports.exodus-privacy.eu.org/reports/51731/icon</bitbar.image>
# <bitbar.dependencies>python, requests, json</bitbar.dependencies>

import requests
import json
from dataclasses import dataclass

static_type = "item"

live_types = {
    "topstories":  ("Top Stories", 'https://news.ycombinator.com/'),
    "newstories":  ("New Stories", 'https://news.ycombinator.com/newest'),
    "beststories": ("Best Stories", 'https://news.ycombinator.com/best'),
    "askstories":  ("Ask Stories", 'https://news.ycombinator.com/ask'),
    "showstories": ("Show Stories", 'https://news.ycombinator.com/show'),
    "jobstories":  ("Job Stories", 'https://news.ycombinator.com/jobs')
}


@dataclass
class Article:
    id:    str
    title: str
    by:    str
    url:   str
    time:  str

    def __str__(self):
        return "{} by: {} | href={}".format(self.title, self.by, self.url)


class Client:
    def __init__(self, type):
        self.type = type

    def fetch_data(self):
        res, ret = self.ids_to_json(requests.get(live_data(self.type)).json()[:10]), []
        for article in res:
            url = article["url"] if "url" in article else ""

            if self.type == 'askstories':
                url = 'https://news.ycombinator.com/item?id={}'.format(article["id"])

            ret.append(
                Article(
                    article["id"],
                    article["title"],
                    article["by"],
                    url,
                    article["time"],
                )
            )
        return ret

    def ids_to_json(self, data):
        return [requests.get(static_data(id, static_type)).json() for id in data]


def live_data(type):
    return "https://hacker-news.firebaseio.com/v0/{}.json?print=pretty".format(type)


def static_data(id, type):
    return "https://hacker-news.firebaseio.com/v0/{}/{}.json?print=pretty".format(type, id)


def separator(level):
    return '{}'.format('-'*level)


def main():
    print('Hacker News\n---')
    for type in live_types:
        client = Client(type)
        articles = client.fetch_data()

        print(live_types[type][0])
        for article in articles:
            print('{}{}'.format(separator(2), article))

        print(separator(5))

        print('{}Hacker News - {} | href={}'.format(
            separator(2),
            live_types[type][0],
            live_types[type][1])
        )

    print(separator(3))
    print('Hacker News - Front Page | href={}'.format('https://news.ycombinator.com/news'))

if __name__ == "__main__":
    main()
