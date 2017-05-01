#!/usr/bin/python
# coding=utf-8
#
# <bitbar.title>Alexa rank checker</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Luca Cappelletti</bitbar.author>
# <bitbar.author.github>LucaCappelletti94</bitbar.author.github>
# <bitbar.desc>Displays Alexa rank from given websites</bitbar.desc>
# <bitbar.image>http://i.imgur.com/30YjoCF.jpg</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# by Luca Cappelletti

import urllib
import sys
import xml.etree.ElementTree as ET
import time
import json
import os
from urlparse import urlparse

reload(sys)
sys.setdefaultencoding('utf8')


class Alexa:
    api_url = "http://data.alexa.com/data"
    is_online_test_url = "http://google.com"
    cache_file_dir = "/tmp/alexa_rank_cache"
    cache_file_name = "/tmp/alexa_rank_cache/alexa_cache.json"
    popularity_icon = " 🌐 "
    black_popularity_icon = " ♥ "
    delta_icon = " ⇕ "
    default_flag_icon = "⚑"
    flags_icons = {
        "AU": "🇦🇺",
        "CA": "🇨🇦",
        "CH": "🇨🇭",
        "CN": "🇨🇳",
        "DE": "🇩🇪",
        "ES": "🇪🇸",
        "FR": "🇫🇷",
        "GB": "🇬🇧",
        "IL": "🇮🇱",
        "IT": "🇮🇹",
        "JP": "🇯🇵",
        "KN": "🇰🇳",
        "KR": "🇰🇷",
        "NO": "🇳🇴",
        "NP": "🇳🇵",
        "PS": "🇵🇸",
        "QA": "🇶🇦",
        "RU": "🇷🇺",
        "SS": "🇸🇸",
        "US": "🇺🇸"
    }

    OFFLINE = 0
    UNRETRIEVED_DATA = 1
    INVALID_JSON = 2
    UNEXISTING_FILE = 3
    INVALID_URLS = 4
    UNEXISTING_ERROR_ID = 5

    error_messages = {
        OFFLINE: {
            "en": "You are offline.",
            "de": "Du bist Offline.",
            "it": "Sei offline."
        },
        UNRETRIEVED_DATA: {
            "en": "Unable to get data for %s.",
            "it": "Impossibile recuperare dati per %s."
        },
        INVALID_JSON: {
            "en": "The file %s does not contain valid json.",
            "it": "Il file %s non contiene json valido."
        },
        UNEXISTING_FILE: {
            "en": "The file %s does not exist.",
            "it": "Il file %s non esiste."
        },
        INVALID_URLS: {
            "en": "No valid urls were provided.",
            "it": "Non son stati inseriti url validi."
        },
        UNEXISTING_ERROR_ID: {
            "en": "The given error_id %s is not valid",
            "it": "L'error_id %s fornito non è valido"
        }
    }

    website_url_list = []
    alexa_data = {}
    cli = 10
    cache = True
    max_offline_wait = 10
    user_language = "en"
    default_user_language = "en"
    polling_interval = 60 * 60  # One hour
    show_global = True
    show_top_country = True
    show_delta = False
    use_black_icons = False

    def __init__(self, website_url_list, cache=None, polling_interval=None, show_global=None, show_top_country=None, show_delta=None, use_black_icons=None, max_offline_wait=None, user_language=None):

        if cache != None:
            self.cache = cache

        if polling_interval != None:
            self.polling_interval = polling_interval

        if show_global != None:
            self.show_global = show_global

        if show_top_country != None:
            self.show_top_country = show_top_country

        if show_delta != None:
            self.show_delta = show_delta

        if use_black_icons != None:
            self.use_black_icons = use_black_icons

        if max_offline_wait != None:
            self.max_offline_wait = max_offline_wait

        if user_language != None:
            self.user_language = user_language

        for website_url in website_url_list:
            if self.is_valid_url(website_url):
                self.website_url_list.append(website_url)

        if len(self.website_url_list) == 0:
            print(self.get_error_message(self.INVALID_URLS))
            sys.exit()

        self.update()

    @classmethod
    def from_url(cls, website_url, cache=None, polling_interval=None, show_global=None, show_top_country=None, show_delta=None, use_black_icons=None, max_offline_wait=None, user_language=None):
        return cls([website_url], cache, polling_interval, show_global, show_top_country, show_delta, use_black_icons)

    def get_error_message(self, error_id):
        if error_id in self.error_messages:
            if self.user_language in self.error_messages[error_id]:
                return self.error_messages[error_id][self.user_language]
            return self.error_messages[error_id][self.default_user_language]
        print(self.get_error_message(self.UNEXISTING_ERROR_ID), error_id)
        sys.exit(0)

    def get_data(self, website_url, data):
        return self.alexa_data[website_url][data]

    def extract(self, raw_data, key, attribute):
        return raw_data.find(key).get(attribute)

    def extract_delta(self, raw_data):
        try:
            return int(self.extract(raw_data, "RANK", "DELTA"))
        except AttributeError:
            return 0

    def extract_rank(self, raw_data):
        try:
            return int(self.extract(raw_data, "REACH", "RANK"))
        except AttributeError:
            return 0

    def extract_country_rank(self, raw_data):
        try:
            return int(self.extract(raw_data, "COUNTRY", "RANK"))
        except AttributeError:
            return 0

    def extract_country_code(self, raw_data):
        try:
            return self.extract(raw_data, "COUNTRY", "CODE")
        except AttributeError:
            return ""

    def extract_country_name(self, raw_data):
        try:
            return self.extract(raw_data, "COUNTRY", "NAME")
        except AttributeError:
            return ""

    def extract_url(self, raw_data):
        try:
            return self.extract(raw_data, "POPULARITY", "URL").strip("/")
        except AttributeError:
            return ""

    def build_url(self, website_url):
        return self.api_url + "?cli=" + str(self.cli) + "&url=" + website_url

    def read_from_url(self, url):
        try:
            return urllib.urlopen(url).read()
        except IOError:
            return None

    def get_alexa_data(self, website_url):
        request_url = self.build_url(website_url)
        data_xml = self.read_from_url(request_url)
        if data_xml == None:
            print(self.get_error_message(self.UNRETRIEVED_DATA), self.clean_url(website_url))
            sys.exit()
        return ET.ElementTree(ET.fromstring(data_xml)).getroot().find("SD")

    def update_url_data(self, website_url):
        raw_data = self.get_alexa_data(website_url)
        self.alexa_data[website_url] = {
            "delta": self.extract_delta(raw_data),
            "global_rank": self.extract_rank(raw_data),
            "top_country_rank": self.extract_country_rank(raw_data),
            "top_country_code": self.extract_country_code(raw_data),
            "top_country_name": self.extract_country_name(raw_data),
            "url": self.extract_url(raw_data),
            "last_update": time.time()
        }

    def save_data_to_cache(self):
        if not os.path.exists(self.cache_file_dir):
            os.makedirs(self.cache_file_dir)
        with open(self.cache_file_name, 'w') as outfile:
            json.dump(self.alexa_data, outfile)

    def load_data_from_cache(self):
        if os.path.exists(self.cache_file_name):
            with open(self.cache_file_name, 'r') as outfile:
                try:
                    return json.load(outfile)
                except ValueError:
                    os.remove(self.cache_file_name)
                    pass
        return {}

    def is_valid_url(self, website_url):
        test = urlparse(website_url)
        if test.netloc == "":
            return False
        return True

    def is_user_online(self):
        if self.read_from_url(self.is_online_test_url) == None:
            return False
        return True

    def is_url_cached(self, website_url):
        if (website_url in self.alexa_data):
            if(self.get_data(website_url, "last_update") > time.time() - self.polling_interval):
                return True
        return False

    def update(self):
        if self.cache:
            self.alexa_data = self.load_data_from_cache()
        if self.is_user_online():
            for website_url in self.website_url_list:
                if not self.is_url_cached(website_url):
                    self.update_url_data(website_url)
        if(len(self.alexa_data) == 0):
            seconds_waited = 0
            while(not self.is_user_online() and self.max_offline_wait > seconds_waited):
                time.sleep(1)
                seconds_waited = seconds_waited + 1
            if(self.max_offline_wait > seconds_waited):
                self.update()
            else:
                print(self.get_error_message(self.OFFLINE))
                sys.exit()
        if self.cache:
            self.save_data_to_cache()

    def get_global_alexa_rank(self, website_url):
        return self.get_data(website_url, "global_rank")

    def get_top_country_alexa_rank(self, website_url):
        return self.get_data(website_url, "top_country_rank")

    def get_alexa_delta(self, website_url):
        return self.get_data(website_url, "delta")

    def get_top_country_alexa_code(self, website_url):
        return self.get_data(website_url, "top_country_code")

    def get_popularity_icon(self):
        if self.use_black_icons == False:
            return self.popularity_icon
        return self.black_popularity_icon

    def get_flag_icon(self, code):
        if self.use_black_icons == False and code in self.flags_icons:
            return self.flags_icons[code]
        return self.default_flag_icon + " " + code + ": "

    def clean_url(self, website_url):
        if website_url not in self.alexa_data or self.alexa_data[website_url]["url"] == "":
            website_url = website_url.replace("http://", "")
            website_url = website_url.replace("https://", "")
            website_url = website_url.replace("www.", "")
            return website_url
        return self.alexa_data[website_url]["url"]

    def build_bitbar(self):
        for website_url in self.website_url_list:
            bitbar = self.clean_url(website_url) + ":"
            if self.show_global:
                bitbar = bitbar + self.get_popularity_icon() + str(self.get_global_alexa_rank(website_url)) + " "
            if self.show_top_country:
                bitbar = bitbar + self.get_flag_icon(self.get_top_country_alexa_code(website_url)) + \
                    str(self.get_top_country_alexa_rank(website_url))
            if self.show_delta:
                bitbar = bitbar + self.delta_icon + str(self.get_alexa_delta(website_url))
            print(bitbar)
            print("---")

###################
#  USAGE EXAMPLES #
###################

# With single url
# myAlexa = Alexa.from_url("https://twitchtimer.com")

# Full parameters bonanza
#
# cache: put this to true to cache to /tmp/alexa_rank_cache/alexa_cache.json
# polling_interval: seconds of duration of caching file
# show_global: put this to true to show the global rank
# show_top_country: put this to true to show the top country rank
# show_delta: put this to true to show how the website rank has varied in the last period
# use_black_icons: for those who do not like color in their menu bar
# max_offline_wait: how many seconds should be waited brefore triyng again to read data and eventually show an "You are offline" error.
# user_language: the language in which errors and messages should be shown, if available
#
# myAlexa = Alexa(["twitchtimer.com", "google.com"], cache=True, polling_interval=60*60, show_global=True, show_top_country=True, show_delta=False, use_black_icons=False, max_offline_wait=15, user_language="en")

myAlexa = Alexa(["https://twitchtimer.com"], use_black_icons=True, show_top_country=False)
myAlexa.build_bitbar()
