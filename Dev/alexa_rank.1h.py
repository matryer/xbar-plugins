#!/usr/bin/python
# coding=utf-8
#
# <xbar.title>Alexa rank checker</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Luca Cappelletti</xbar.author>
# <xbar.author.github>LucaCappelletti94</xbar.author.github>
# <xbar.desc>Displays Alexa rank from given websites</xbar.desc>
# <xbar.image>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</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
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
