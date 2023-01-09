#!/usr/bin/env python3

# <xbar.title>FCBarcelona.dk</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Tuk Bredsdorff</xbar.author>
# <xbar.author.github>tiktuk</xbar.author.github>
# <xbar.desc>News from fcbarcelona.dk.</xbar.desc>
# <xbar.image>http://tiktuk.net/sky/fcb-bitbar.png</xbar.image>
# <xbar.dependencies>python3, requests, BeautifulSoup4</xbar.dependencies>

import requests
from bs4 import BeautifulSoup

URL = 'http://fcbarcelona.dk'
DIVIDER = '---\n'

if __name__ == '__main__':
    try:
        r = requests.get(URL)

        if r.ok:
            print('⚽️')
            print(DIVIDER)

            soup = BeautifulSoup(r.text, 'html.parser')
            main_article_link = soup.find(id='main_story').find('a')
            article_links = soup.find(id='article_list').find_all('a')
            article_links.insert(0, main_article_link)

            for a in article_links:
                dest = f'{URL}/{a.attrs["href"]}'
                text = a.text.strip().replace('\xa0\xa0-\xa0\xa0', ': ')
                print(f'{text} | href={dest}')
    except requests.exceptions.ConnectionError:
        print('😳')
        print(DIVIDER)
        print('Could not connect to %s' % URL)
