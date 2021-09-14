#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Quran Daily</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Saif Ali Khan</xbar.author>
# <xbar.author.github>saifat29</xbar.author.github>
# <xbar.desc>Displays a verse from the Holy Quran</xbar.desc>
# <xbar.image>https://i.imgur.com/9uJiRVg.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

import os
import json
import re

from urllib2 import quote, urlopen

BASE_URL = "https://api.quran.com/api/v4"


def ayah_en(verse_key):
    ayah_url = BASE_URL + \
        "/quran/translations/20?verse_key=" + quote(verse_key)

    r = urlopen(ayah_url, timeout=10)
    return json.loads(r.read())


def ayah_random():
    ayah_url = BASE_URL + \
        "/verses/random?language=en&words=false"

    r = urlopen(ayah_url, timeout=10)
    return json.loads(r.read())


def remove_footnotes(verse):
    cleanr = re.compile('<.*>')
    raw_verse = re.sub(cleanr, '', verse)
    return raw_verse


def break_in_lines(chars_limit, s):
    target_lines = (len(s) / chars_limit)
    if (len(s) % chars_limit) > 0:
        target_lines += 1

    lines = []
    for i in range(0, target_lines):
        lines.append(s[(i * (chars_limit + 1)) - i: (i+1) * chars_limit])

    return lines


def set_color(s):
    if os.environ.get('XBARDarkMode') == 'true':
        s += "| ansi=false | color=white"
    elif os.environ.get('XBARDarkMode') == 'false':
        s += "| ansi=false | color=black"
    else:
        s += "| ansi=false"
    return s


if __name__ == '__main__':
    print('القرآن')
    print("---")
    verse_key = ayah_random()['verse']['verse_key']
    verse = remove_footnotes(ayah_en(verse_key)['translations'][0]['text'])
    verse += " - " + verse_key

    verse_list = break_in_lines(50, verse)

    output_verse = ''
    for i, v in enumerate(verse_list):
        output_verse += set_color(v) + "\n"

    print(output_verse.strip())

    print("---")

    quran_dot_com = "https://quran.com/"+verse_key.replace(":", "/")
    print("Open on Quran.com | href="+quran_dot_com)
